program Pazzle15;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainP15 in 'uMainP15.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
