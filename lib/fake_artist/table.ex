defmodule FakeArtist.Table do
  use GenServer
  require Logger

  # Public API
  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def update_name_tag(pid, {id, name_tag}) do
    GenServer.call(pid, {:update_name_tag, {id, name_tag}})
  end

  def add_self(table_pid) do
    GenServer.call(table_pid, :add_self)
  end

  # Server Callbacks
  def init([name]) do
    {:ok, {%{}, name, 0}}
  end

  def handle_call({:update_name_tag, {id, name_tag}}, _from, {id_map, table_name, user_count}) do
    id_map = Map.put(id_map, id, name_tag)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", %{
      names: Map.values(id_map)
    })

    {:reply, :ok, {id_map, table_name, user_count}}
  end

  def handle_call(:add_self, {from_pid, _}, {id_map, table_name, user_count}) do
    Logger.info(fn -> "started monitoring #{inspect(from_pid)}" end)
    Process.monitor(from_pid)
    Logger.info(fn -> "player count increased from #{user_count} to #{user_count + 1}" end)
    {:reply, :ok, {id_map, table_name, user_count + 1}}
  end

  def handle_info({:DOWN, _ref, :process, _from, _reason}, {id_map, table_name, user_count}) do
    Logger.info(fn -> "table lost connection." end)
    Logger.info(fn -> "player count decreased from #{user_count} to #{user_count - 1}" end)
    user_count = user_count - 1

    if user_count == 0 do
      Logger.info(fn -> "suicide" end)
      {:stop, :shutdown, {%{}, "", 0}}
    else
      {:noreply, {id_map, table_name, user_count}}
    end
  end
end
