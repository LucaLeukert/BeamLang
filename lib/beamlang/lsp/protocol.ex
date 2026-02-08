defmodule BeamLang.LSP.Protocol do
  @moduledoc false

  alias ElixirLsp.Message

  @spec read_message() :: {:ok, map()} | :eof
  def read_message do
    case read_headers(%{}) do
      :eof ->
        :eof

      {:ok, headers} ->
        case Map.get(headers, "content-length") do
          nil ->
            :eof

          length_str ->
            length = String.to_integer(String.trim(length_str))

            case IO.binread(:stdio, length) do
              :eof ->
                :eof

              body when is_binary(body) ->
                BeamLang.LSP.Debug.log(fn -> "recv: " <> body end)
                decode_message(body)

              _ ->
                :eof
            end
        end
    end
  end

  @spec send_response(any(), any()) :: :ok
  def send_response(id, result) do
    send_message(%{"jsonrpc" => "2.0", "id" => id, "result" => result})
  end

  @spec send_error(any(), integer(), binary()) :: :ok
  def send_error(id, code, message) do
    send_message(%{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{"code" => code, "message" => message}
    })
  end

  @spec send_notification(binary(), map()) :: :ok
  def send_notification(method, params) when is_binary(method) and is_map(params) do
    send_message(%{"jsonrpc" => "2.0", "method" => method, "params" => params})
  end

  @spec send_message(map()) :: :ok
  def send_message(payload) when is_map(payload) do
    payload
    |> map_to_message()
    |> ElixirLsp.encode()
    |> then(&IO.iodata_to_binary/1)
    |> then(fn framed ->
      BeamLang.LSP.Debug.log(fn -> "send: " <> extract_body(framed) end)
      IO.binwrite(:stdio, framed)
    end)
  end

  defp read_headers(acc) do
    case IO.binread(:stdio, :line) do
      :eof ->
        :eof

      "\r\n" ->
        {:ok, acc}

      "\n" ->
        {:ok, acc}

      line ->
        line = String.trim_trailing(line)

        case String.split(line, ":", parts: 2) do
          [key, value] ->
            read_headers(Map.put(acc, String.downcase(String.trim(key)), String.trim(value)))

          _ ->
            read_headers(acc)
      end
    end
  end

  defp decode_message(body) do
    framed = ["Content-Length: ", Integer.to_string(byte_size(body)), "\r\n\r\n", body]

    case ElixirLsp.recv(IO.iodata_to_binary(framed)) do
      {:ok, [message], _rest_state} ->
        {:ok, Message.to_map(message)}

      {:ok, [message | _], _rest_state} ->
        {:ok, Message.to_map(message)}

      {:ok, [], _rest_state} ->
        {:error, :invalid_json}

      {:error, _reason, _rest_state} ->
        {:error, :invalid_json}
    end
  end

  defp map_to_message(%{"jsonrpc" => "2.0", "id" => id, "result" => result}) do
    ElixirLsp.response(id, result)
  end

  defp map_to_message(%{"jsonrpc" => "2.0", "id" => id, "error" => %{"code" => code, "message" => message} = error}) do
    ElixirLsp.error_response(id, code, message, Map.get(error, "data"))
  end

  defp map_to_message(%{"jsonrpc" => "2.0", "method" => method, "params" => params}) do
    ElixirLsp.notification(method, params)
  end

  defp map_to_message(%{"jsonrpc" => "2.0", "method" => method}) do
    ElixirLsp.notification(method, nil)
  end

  defp map_to_message(map) do
    case Message.from_map(Map.put_new(map, "jsonrpc", "2.0")) do
      {:ok, message} -> message
      {:error, _reason} -> ElixirLsp.notification("window/logMessage", %{"type" => 1, "message" => "invalid outgoing LSP payload"})
    end
  end

  defp extract_body(framed) when is_binary(framed) do
    case :binary.split(framed, "\r\n\r\n") do
      [_header, body] -> body
      _ -> framed
    end
  end
end
