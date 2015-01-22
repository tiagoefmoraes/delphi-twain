program SimpleExample;

uses
  Forms,
  SimpleExampleForm in 'SimpleExampleForm.pas' {Form1};

{$R *.RES}

begin
  //Application.MainFormOnTaskBar := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
