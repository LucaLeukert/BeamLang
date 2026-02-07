defmodule BeamLang.LSP.Protocol do
  @moduledoc false

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

                case Jason.decode(body) do
                  {:ok, decoded} -> {:ok, decoded}
                  {:error, _reason} -> {:error, :invalid_json}
                end

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
    json = Jason.encode!(payload)
    BeamLang.LSP.Debug.log(fn -> "send: " <> json end)
    IO.binwrite(:stdio, "Content-Length: #{byte_size(json)}\r\n\r\n")
    IO.binwrite(:stdio, json)
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
end
