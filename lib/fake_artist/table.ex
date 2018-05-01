defmodule FakeArtist.Table do
  use GenServer

  def init(args) do
    {:ok, args}
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end
end
