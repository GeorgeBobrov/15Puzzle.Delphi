program Pazzle15;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormPuzzle15 in 'uFormPuzzle15.pas' {FormPuzzle15};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFormPuzzle15, FormPuzzle15);
  Application.Run;
end.
