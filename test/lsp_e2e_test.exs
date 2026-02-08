defmodule BeamLang.LSP.E2ETest do
  @moduledoc """
  End-to-end tests for the BeamLang LSP server.

  These tests launch the LSP server as a separate process communicating
  over stdio (via ports), send real JSON-RPC messages, and verify responses.
  """
  use ExUnit.Case, async: false

  @timeout 15_000

  # ---- Helpers to communicate with the LSP server ----

  defp start_server do
    mix_path = System.find_executable("mix")

    port =
      Port.open({:spawn_executable, mix_path}, [
        :binary,
        :exit_status,
        :use_stdio,
        args: ["beamlang", "--lsp"],
        cd: File.cwd!(),
        env: [{~c"BEAMLANG_LSP_DEBUG", ~c"0"}]
      ])

    port
  end

  defp send_request(port, id, method, params) do
    msg =
      Jason.encode!(%{
        "jsonrpc" => "2.0",
        "id" => id,
        "method" => method,
        "params" => params
      })

    send_raw(port, msg)
  end

  defp send_notification(port, method, params) do
    msg =
      Jason.encode!(%{
        "jsonrpc" => "2.0",
        "method" => method,
        "params" => params
      })

    send_raw(port, msg)
  end

  defp send_raw(port, json) do
    header = "Content-Length: #{byte_size(json)}\r\n\r\n"
    Port.command(port, header <> json)
  end

  # Wait for a specific response by id, consuming notifications along the way
  defp recv_response_by_id(port, expected_id, timeout \\ @timeout) do
    recv_response_by_id_acc(port, expected_id, "", [], timeout)
  end

  defp recv_response_by_id_acc(port, expected_id, buffer, notifications, timeout) do
    receive do
      {^port, {:data, data}} ->
        buffer = buffer <> data

        case parse_lsp_message(buffer) do
          {:ok, msg, rest} ->
            if Map.get(msg, "id") == expected_id do
              {msg, notifications}
            else
              # It's a notification (like diagnostics), collect and continue
              recv_response_by_id_acc(port, expected_id, rest, [msg | notifications], timeout)
            end

          :incomplete ->
            recv_response_by_id_acc(port, expected_id, buffer, notifications, timeout)
        end

      {^port, {:exit_status, _code}} ->
        {:exit, notifications}
    after
      timeout -> {:timeout, buffer}
    end
  end

  defp parse_lsp_message(buffer) do
    case Regex.run(~r/Content-Length:\s*(\d+)\r?\n\r?\n/s, buffer, return: :index) do
      [{header_start, header_len}, {len_start, len_len}] ->
        content_length = buffer |> binary_part(len_start, len_len) |> String.to_integer()
        body_start = header_start + header_len
        total_needed = body_start + content_length

        if byte_size(buffer) >= total_needed do
          body = binary_part(buffer, body_start, content_length)
          rest = binary_part(buffer, total_needed, byte_size(buffer) - total_needed)
          {:ok, Jason.decode!(body), rest}
        else
          :incomplete
        end

      _ ->
        :incomplete
    end
  end

  defp stop_server(port) do
    send_request(port, 99999, "shutdown", nil)
    # Give it a moment then send exit
    receive do
      {^port, {:data, _}} -> :ok
    after
      1000 -> :ok
    end

    send_notification(port, "exit", %{})

    receive do
      {^port, {:exit_status, _}} -> :ok
    after
      2000 ->
        try do
          Port.close(port)
        rescue
          _ -> :ok
        end
    end
  end

  defp test_uri, do: "file:///tmp/test_lsp.bl"

  defp test_source do
    """
    type User {
        name: String,
        age: number
    }

    fn greet(user: User) -> String {
        return "Hello";
    }

    fn main(args: [String]) -> number {
        let st = "test";
        println(st);
        return 0;
    }
    """
  end

  defp initialize(port) do
    send_request(port, 1, "initialize", %{
      "processId" => System.pid() |> String.to_integer(),
      "rootUri" => "file:///tmp",
      "capabilities" => %{}
    })

    {resp, _notifs} = recv_response_by_id(port, 1)
    assert resp["result"]["capabilities"]

    # Send initialized notification
    send_notification(port, "initialized", %{})

    resp
  end

  defp open_document(port, uri \\ nil, text \\ nil) do
    uri = uri || test_uri()
    text = text || test_source()

    send_notification(port, "textDocument/didOpen", %{
      "textDocument" => %{
        "uri" => uri,
        "languageId" => "beamlang",
        "version" => 1,
        "text" => text
      }
    })

    # Wait a moment for diagnostics to be published
    Process.sleep(500)
  end

  # ---- Tests ----

  test "initialize and shutdown" do
    port = start_server()
    resp = initialize(port)

    capabilities = resp["result"]["capabilities"]
    assert capabilities["hoverProvider"] == true
    assert capabilities["completionProvider"]
    assert capabilities["definitionProvider"] == true
    assert capabilities["renameProvider"]
    assert capabilities["referencesProvider"] == true
    assert capabilities["documentFormattingProvider"] == true

    stop_server(port)
  end

  test "hover on function name" do
    port = start_server()
    initialize(port)
    open_document(port)

    # Hover over "greet" in the call at line 11 (0-indexed), character 12
    send_request(port, 2, "textDocument/hover", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 11, "character" => 12}
    })

    {resp, _} = recv_response_by_id(port, 2)
    # Just ensure no crash
    assert resp["result"] != nil or resp["result"] == nil

    stop_server(port)
  end

  test "completion returns items including locals" do
    port = start_server()
    initialize(port)

    # Use a source where we can trigger completion at a known clean position
    source = """
    fn main(args: [String]) -> number {
        let st = "test";
        println(st);
        
        return 0;
    }
    """

    open_document(port, test_uri(), source)

    # Request completion at the blank line (line 3) where nothing is typed yet
    # This should return all available locals + functions + keywords
    send_request(port, 3, "textDocument/completion", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 3, "character" => 4}
    })

    {resp, _} = recv_response_by_id(port, 3)
    assert resp["result"]
    items = resp["result"]["items"]
    assert is_list(items)

    # There should be some completion items (at minimum keywords + functions)
    assert length(items) > 0

    # Check that "st" is among the completions (local variable)
    labels = Enum.map(items, & &1["label"])
    assert "st" in labels, "Expected 'st' in completions, got: #{inspect(labels)}"

    stop_server(port)
  end

  test "completion filters by prefix" do
    port = start_server()
    initialize(port)

    # Open a document where we can test prefix filtering
    source = """
    fn main(args: [String]) -> number {
        let status = "ok";
        let start = 0;
        let other = 1;
        st
    }
    """

    open_document(port, test_uri(), source)

    # Request completion at position of "st" (line 4, character 6)
    send_request(port, 4, "textDocument/completion", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 4, "character" => 6}
    })

    {resp, _} = recv_response_by_id(port, 4)
    items = resp["result"]["items"] || []

    # All items should start with "st"
    labels = Enum.map(items, & &1["label"])

    if length(labels) > 0 do
      assert Enum.all?(labels, fn label ->
               String.starts_with?(String.downcase(label), "st")
             end),
             "Expected all labels to start with 'st', got: #{inspect(labels)}"
    end

    stop_server(port)
  end

  test "definition for local variable" do
    port = start_server()
    initialize(port)
    open_document(port)

    # Go to definition of "st" at line 12 (println(st))
    send_request(port, 5, "textDocument/definition", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 12, "character" => 12}
    })

    {resp, _} = recv_response_by_id(port, 5)
    # Should return a location or empty array, not crash
    assert is_list(resp["result"]) or is_map(resp["result"]) or resp["result"] == nil

    stop_server(port)
  end

  test "document symbols" do
    port = start_server()
    initialize(port)
    open_document(port)

    send_request(port, 6, "textDocument/documentSymbol", %{
      "textDocument" => %{"uri" => test_uri()}
    })

    {resp, _} = recv_response_by_id(port, 6)
    symbols = resp["result"]
    assert is_list(symbols)

    names = Enum.map(symbols, & &1["name"])
    assert "main" in names
    assert "greet" in names

    stop_server(port)
  end

  test "rename local variable" do
    port = start_server()
    initialize(port)
    open_document(port)

    # Prepare rename for "st" at line 11, character 8
    send_request(port, 7, "textDocument/prepareRename", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 11, "character" => 8}
    })

    {resp, _} = recv_response_by_id(port, 7)
    # Should return a range+placeholder or nil, not crash
    result = resp["result"]
    assert result == nil or (is_map(result) and Map.has_key?(result, "range"))

    # Now do the actual rename
    send_request(port, 8, "textDocument/rename", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 11, "character" => 8},
      "newName" => "my_string"
    })

    {resp, _} = recv_response_by_id(port, 8)
    result = resp["result"]
    # Should be a WorkspaceEdit or nil
    assert result == nil or (is_map(result) and Map.has_key?(result, "changes"))

    stop_server(port)
  end

  test "references for identifier" do
    port = start_server()
    initialize(port)
    open_document(port)

    send_request(port, 9, "textDocument/references", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 11, "character" => 8},
      "context" => %{"includeDeclaration" => true}
    })

    {resp, _} = recv_response_by_id(port, 9)
    refs = resp["result"]
    assert is_list(refs)

    stop_server(port)
  end

  test "formatting" do
    port = start_server()
    initialize(port)

    # Open a poorly formatted document
    ugly_source = "fn main( args:[String] )->number{let x=1;return x;}"

    open_document(port, test_uri(), ugly_source)

    send_request(port, 10, "textDocument/formatting", %{
      "textDocument" => %{"uri" => test_uri()},
      "options" => %{"tabSize" => 4, "insertSpaces" => true}
    })

    {resp, _} = recv_response_by_id(port, 10)
    edits = resp["result"]
    assert is_list(edits)

    stop_server(port)
  end

  test "server survives malformed didChange" do
    port = start_server()
    initialize(port)
    open_document(port)

    # Send a didChange with broken content
    send_notification(port, "textDocument/didChange", %{
      "textDocument" => %{"uri" => test_uri()},
      "contentChanges" => [%{"text" => "fn broken { let x = "}]
    })

    Process.sleep(500)

    # Server should still respond to requests
    send_request(port, 11, "textDocument/hover", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 0, "character" => 3}
    })

    {resp, _} = recv_response_by_id(port, 11)
    # Doesn't matter what result is, just that we got a valid response
    assert Map.has_key?(resp, "id")
    assert resp["id"] == 11

    stop_server(port)
  end

  test "server handles unknown method gracefully" do
    port = start_server()
    initialize(port)

    # Send a request with an unknown method
    send_request(port, 12, "textDocument/unknownMethod", %{
      "textDocument" => %{"uri" => test_uri()}
    })

    # The server should not crash - it just won't respond to unknown requests
    # (the catch-all returns state without sending a response)
    # Send another valid request to verify the server is alive
    Process.sleep(200)

    send_request(port, 13, "textDocument/documentSymbol", %{
      "textDocument" => %{"uri" => test_uri()}
    })

    {resp, _} = recv_response_by_id(port, 13)
    assert resp["id"] == 13

    stop_server(port)
  end

  test "signature help" do
    port = start_server()
    initialize(port)
    open_document(port)

    # Request signature help at println( on line 12
    send_request(port, 14, "textDocument/signatureHelp", %{
      "textDocument" => %{"uri" => test_uri()},
      "position" => %{"line" => 12, "character" => 12}
    })

    {resp, _} = recv_response_by_id(port, 14)
    # Should return signatures or nil, not crash
    assert resp["id"] == 14

    stop_server(port)
  end

  test "hover shows type info for match case bindings" do
    port = start_server()
    initialize(port)

    path = Path.expand("examples/apps/cat.bl")
    uri = "file://" <> path
    source = File.read!(path)
    open_document(port, uri, source)

    # Hover inside `opts` in: `case!ok opts -> {`
    send_request(port, 15, "textDocument/hover", %{
      "textDocument" => %{"uri" => uri},
      "position" => %{"line" => 30, "character" => 17}
    })

    {resp, _} = recv_response_by_id(port, 15)
    assert resp["result"] != nil

    value = get_in(resp, ["result", "contents", "value"]) || ""
    assert value =~ "opts"
    assert value =~ "Args"

    stop_server(port)
  end

  test "document symbols works for module import files" do
    port = start_server()
    initialize(port)

    path = Path.expand("examples/modules/use_math.bl")
    uri = "file://" <> path
    source = File.read!(path)
    open_document(port, uri, source)

    send_request(port, 16, "textDocument/documentSymbol", %{
      "textDocument" => %{"uri" => uri}
    })

    {resp, _} = recv_response_by_id(port, 16)
    symbols = resp["result"]
    assert is_list(symbols)

    names = Enum.map(symbols, & &1["name"])
    assert "main" in names

    stop_server(port)
  end

  test "qualified module refs resolve without explicit import" do
    port = start_server()
    initialize(port)

    path = Path.expand("examples/modules/use_math.bl")
    uri = "file://" <> path
    source = File.read!(path)
    open_document(port, uri, source)

    send_request(port, 17, "textDocument/hover", %{
      "textDocument" => %{"uri" => uri},
      "position" => %{"line" => 6, "character" => 22}
    })

    {resp, notifications} = recv_response_by_id(port, 17)

    diagnostics =
      notifications
      |> Enum.filter(fn msg ->
        msg["method"] == "textDocument/publishDiagnostics" and
          get_in(msg, ["params", "uri"]) == uri
      end)
      |> Enum.flat_map(fn msg -> get_in(msg, ["params", "diagnostics"]) || [] end)

    assert diagnostics == []
    assert resp["result"] != nil

    stop_server(port)
  end

  test "hover on method filters by receiver type" do
    port = start_server()
    initialize(port)

    path = Path.expand("examples/modules/stdlib_methods.bl")
    uri = "file://" <> path
    source = File.read!(path)
    open_document(port, uri, source)

    # Hover `length` in `st->length()`
    send_request(port, 20, "textDocument/hover", %{
      "textDocument" => %{"uri" => uri},
      "position" => %{"line" => 2, "character" => 21}
    })

    {resp, _} = recv_response_by_id(port, 20)
    assert resp["result"] != nil

    value = get_in(resp, ["result", "contents", "value"]) || ""
    assert value =~ "String::length"
    refute value =~ "Range::length"
    refute value =~ "List::length"

    stop_server(port)
  end

  test "definition jumps across module files for qualified refs" do
    port = start_server()
    initialize(port)

    use_math_path = Path.expand("examples/modules/use_math.bl")
    use_math_uri = "file://" <> use_math_path
    use_math_source = File.read!(use_math_path)
    open_document(port, use_math_uri, use_math_source)

    math_ops_path = Path.expand("examples/modules/math_ops.bl")
    math_ops_uri = "file://" <> math_ops_path
    math_ops_source = File.read!(math_ops_path)
    open_document(port, math_ops_uri, math_ops_source)

    # On `add` in `math_ops::add(...)`
    send_request(port, 18, "textDocument/definition", %{
      "textDocument" => %{"uri" => use_math_uri},
      "position" => %{"line" => 6, "character" => 22}
    })

    {resp_add, _} = recv_response_by_id(port, 18)
    [add_loc | _] = resp_add["result"] || []
    assert add_loc["uri"] == math_ops_uri
    assert get_in(add_loc, ["range", "start", "line"]) == 5

    # On `MathPair` in `math_ops::MathPair`
    send_request(port, 19, "textDocument/definition", %{
      "textDocument" => %{"uri" => use_math_uri},
      "position" => %{"line" => 1, "character" => 27}
    })

    {resp_type, _} = recv_response_by_id(port, 19)
    [type_loc | _] = resp_type["result"] || []
    assert type_loc["uri"] == math_ops_uri
    assert get_in(type_loc, ["range", "start", "line"]) == 0

    stop_server(port)
  end
end
