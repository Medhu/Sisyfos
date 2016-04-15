with AUnit.Run;
with AUnit.Reporter.Text;
with Ts_Streaming;

---------------
-- Harness10 --
---------------

procedure Harness10 is

   procedure Run is new AUnit.Run.Test_Runner (Ts_Streaming.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run (Reporter);
end Harness10;
