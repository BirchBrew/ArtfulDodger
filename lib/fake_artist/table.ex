defmodule FakeArtist.Table do
  use GenServer

  def new do
    GenServer.start_link(__MODULE__, 0)
  end

  def init(args) do
    {:ok, args}
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end
end
