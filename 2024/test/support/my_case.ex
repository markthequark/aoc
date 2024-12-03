defmodule MyCase do
  use ExUnit.CaseTemplate

  setup do
    Mox.stub(MockFile, :read!, &File.read!/1)
    :ok
  end
end
