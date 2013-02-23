unit SetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, jpeg;

type
  TForm7 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Image1: TImage;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public

  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

uses madShell, IniFiles;



Procedure ExtractKeyFile (ResName: String; ToFileName: String);
begin
  With TResourceStream.create(hinstance, ResName, 'PE_FILE') do
  try
    SaveToFile(ToFileName);
  finally
    Free;
  end;
end;

Procedure RecordPathToIni (IniFileName, Ident, Path: String);
begin
  ForceDirectories (ExtractFilePath(IniFileName));
  With TIniFile.Create (IniFileName) do
  Try
    WriteString ('KeyFile', Ident, Path);
  finally
    Free;
  end;  
end;

function ReadDbgLogerINI (IniFileName, Ident: String): String;
begin
  Result := '';
  With TIniFile.Create (IniFileName) do
  Try
    Result := ReadString ('KeyFile', Ident, '');
  finally
    Free;
  end;  
end;

procedure MakeShortCutToDesktop (ExeFile: String);
var
  FileName: String;
  DesktopLnkFileName: String;
  ShortCut: IShortCut;
begin
  if not GetSpecialFolder (sfDesktopDir, DesktopLnkFileName) then
  begin
    ShowMessage ('获取快捷方式路径错误');
    Exit;
  end;
  FileName := ExtractFileName (ExeFile);
  FileName := ChangeFileExt (FileName, '.lnk');
  DesktopLnkFileName := DesktopLnkFileName + '\' + FileName;

  ShortCut := NewShortCut (ExeFile);
  ShortCut.Save(DesktopLnkFileName);
  ShortCut := nil;  
end;

procedure TForm7.Button1Click(Sender: TObject);
var
  MainExe, FileName: String;
  IniFileName: String;
begin
  if self.Button1.Caption = '退出' then
  begin
    Application.Terminate;
    Exit;
  end;

  ForceDirectories (self.Edit1.Text);

  if not GetSpecialFolder (sfAllUsersAppData, IniFileName) then
  begin
    ShowMessage ('获取配置文件路径错误');
    Exit;
  end;
  IniFileName := IniFileName + '\ANDSoft\DbgLoger.ini';   

  //释放pe文件
  MainExe := self.Edit1.Text + 'DbgLoger.exe';
  ExtractKeyFile ('DbgLogerEXE', MainExe);
  RecordPathToIni (IniFileName, 'DbgLogerEXE', MainExe);
  MakeShortCutToDesktop (MainExe);

  FileName := self.Edit1.Text + 'DbgHost.exe';
  ExtractKeyFile ('DbgHostEXE', FileName);
  RecordPathToIni (IniFileName, 'DbgHostEXE', FileName);
  MakeShortCutToDesktop (FileName);

  FileName := self.Edit1.Text + 'DbgLogerDLL.dll';
  ExtractKeyFile ('DbgLogerDLL', FileName);
  RecordPathToIni (IniFileName, 'DbgLogerDLL', FileName);

  FileName := self.Edit1.Text + 'LogService.dll';
  ExtractKeyFile ('LOGDLL', FileName);
  RecordPathToIni (IniFileName, 'LOGDLL', FileName);  

  FileName := self.Edit1.Text + 'sqlite3.dll';
  ExtractKeyFile ('SQLITE3LIB', FileName);
  RecordPathToIni (IniFileName, 'SQLITE3LIB', FileName);

  RecordPathToIni (IniFileName, 'DbgLogerINI', IniFileName);

  self.StatusBar1.SimpleText := '安装完毕!!!!!!!!!!!!!!!';
  self.Button1.Caption := '退出';
end;

procedure TForm7.FormShow(Sender: TObject);
var
  Dir: String;
begin
  self.StatusBar1.SimplePanel := True;

  Dir := 'C:\';
  if not GetSpecialFolder (sfProgramFiles, Dir) then
  begin
    ShowMessage ('获取ProgramFiles路径错误');
    Exit;
  end;

  Dir := Dir + '\ANDSoft\DbgLoger\';
  self.Edit1.Text := Dir;
end;

end.
