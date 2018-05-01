defmodule FakeArtist.DynamicSupervisor do
  use DynamicSupervisor

  def init(_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_link(arg) do
    DynamicSupervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def start_child() do
    DynamicSupervisor.start_child(__MODULE__, FakeArtist.Table)
  end
end
