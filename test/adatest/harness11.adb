with AUnit.Run;
with AUnit.Reporter.Text;
with Ts_Piece_Pieces_Observation;

---------------
-- Harness10 --
---------------

procedure Harness11 is

   procedure Run is new AUnit.Run.Test_Runner (Ts_Piece_Pieces_Observation.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run (Reporter);
end Harness11;
