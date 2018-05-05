defmodule FakeArtist.Hostess do
  use GenServer
  require Logger

  @chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  @length_of_room_name 4

  # Client API
  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: __MODULE__)
  end

  def join_existing_table(table_name) do
    GenServer.call(__MODULE__, {:join_existing_table, table_name})
  end

  def start_new_table() do
    GenServer.call(__MODULE__, :start_new_table)
  end

  def get_table_pid(name) do
    GenServer.call(__MODULE__, {:get_table_pid, name})
  end

  # Server Callbacks
  def init(_args) do
    {:ok, %{}}
  end

  def handle_call({:join_existing_table, table_name}, _from, tables) do
    case try_join_existing(table_name, tables) do
      {:ok, %{pid: pid}} ->
        {:reply, {:ok, pid}, tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def handle_call(:start_new_table, _from, tables) do
    case try_join_new(tables) do
      {:ok, updated_tables, %{name: name, pid: pid}} ->
        Process.monitor(pid)
        {:reply, {:ok, %{name: name, pid: pid}}, updated_tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def handle_call({:get_table_pid, name}, _from, tables) do
    {:reply, Map.get(tables, name), tables}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, tables) do
    Logger.info(fn -> "hostess lost connection to table" end)
    # Maybe we should just have two maps? So we can go both ways in O(1)?
    Logger.info(fn -> "tables before removal: #{inspect(tables)}" end)
    table_name_for_pid = Enum.find(tables, fn {_, val} -> val == pid end) |> elem(0)
    tables = Map.delete(tables, table_name_for_pid)
    Logger.info(fn -> "tables after removal: #{inspect(tables)}" end)
    {:noreply, tables}
  end

  # Helpers
  defp try_join_existing(table_name, tables) do
    if Map.has_key?(tables, table_name) do
      {:ok, %{pid: Map.get(tables, table_name)}}
    else
      {:error, "The room doesn't exist"}
    end
  end

  defp try_join_new(tables) do
    with {:ok, name} <- get_unique_name(tables),
         {:ok, pid} <- create_process(name),
         tables = Map.put(tables, name, pid) do
      {:ok, tables, %{name: name, pid: pid}}
    else
      _ -> {:error, "Couldn't create the room"}
    end
  end

  defp generate_new_name() do
    code_points = String.codepoints(@chars)

    Enum.reduce(1..@length_of_room_name, [], fn _i, acc ->
      [Enum.random(code_points) | acc]
    end)
    |> Enum.join("")
  end

  defp get_unique_name_helper(tables, name \\ "michelangelo", tries \\ 0) do
    unless Map.has_key?(tables, name) do
      {:ok, name}
    else
      if tries > 1000 do
        {:error}
      else
        get_unique_name_helper(tables, generate_new_name(), tries + 1)
      end
    end
  end

  defp get_unique_name(tables) do
    get_unique_name_helper(tables)
  end

  defp create_process(name) do
    {:ok, pid} = FakeArtist.DynamicSupervisor.start_child(name)
    {:ok, pid}
  end
end
