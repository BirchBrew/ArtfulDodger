defmodule FakeArtist.DynamicSupervisor do
  use DynamicSupervisor

  def start_link(arg) do
    DynamicSupervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child() do
    {:ok, pid} = DynamicSupervisor.start_child(__MODULE__, FakeArtist.Table)
    {:ok, pid}
  end
end
