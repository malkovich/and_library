unit MakeMachineForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    Button2: TButton;
    Label2: TLabel;
    Button4: TButton;
    Edit2: TEdit;
    Button1: TButton;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses HardInfosAndComputerIDUnit, ShellWmi, magwmi, smartapi, magsubs1;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  SystemOut: TSystemOut;
begin
  SystemOut := GetHardSys;

  With SystemOut do
  begin
    Memo1.Lines.Add('BbDate=' + StrPas(BbDate));
    Memo1.Lines.Add('BbVendor=' + StrPas(BbVendor));
    Memo1.Lines.Add('BbVersion=' + StrPas(BbVersion));

    Memo1.Lines.Add('sysmodel=' + StrPas(sysmodel));
    Memo1.Lines.Add('sysmanuf=' + StrPas(sysmanuf));
    Memo1.Lines.Add('sysserial=' + StrPas(sysserial));
    Memo1.Lines.Add('sysUUID=' + StrPas(sysUUID));

    Memo1.Lines.Add('mbmodel=' + StrPas(mbmodel));
    Memo1.Lines.Add('mbmanuf=' + StrPas(mbmanuf));
    Memo1.Lines.Add('mbversion=' + StrPas(mbversion));
    Memo1.Lines.Add('mbserial=' + StrPas(mbserial));

    Memo1.Lines.Add('SMBbVersion=' + StrPas(SMBbVersion));
    Memo1.Lines.Add('SMBbRevision=' + StrPas(SMBbRevision));

    Memo1.Lines.Add('cpuname=' + StrPas(cpuname));
    Memo1.Lines.Add('cpusocketname=' + StrPas(cpusocketname));
    Memo1.Lines.Add('cpumanuf=' + StrPas(cpumanuf));
    Memo1.Lines.Add('cpuvendor=' + StrPas(cpuvendor));
    Memo1.Lines.Add('cpuid=' + StrPas(cpuid));
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  StartTick, BeginTick, OffsetTick: DWORD;
begin
  self.Memo1.Clear;
  self.Memo1.Lines.Add ('UserName='+Edit1.Text);
  self.Memo1.Lines.Add ('MachineName='+Edit2.Text);
  
  BeginTick := GetTickCount;
  StartTick := BeginTick;
  self.Memo1.Lines.Add ('BaseBoard='+CallBufferToString(@One_GetBaseBoard));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('BaseBoard='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('SMBiosVersion='+CallBufferToString(@One_GetSMBiosVersion));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('SMBiosVersion='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('BiosName='+CallBufferToString(@One_GetBiosName));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('BiosName='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('BIOSSerialNumber='+CallBufferToString(@One_GetBIOSSerialNumber));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('BIOSSerialNumber='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('FirstDiskInfo='+CallBufferToString(@One_GetFirstDiskInfo));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('FirstDiskInfo='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('ProcessorInfo='+CallBufferToString(@One_GetProcessorInfo));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('ProcessorInfo='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('PhysicalMemoryInfo='+CallBufferToString(@One_GetPhysicalMemoryInfo));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('PhysicalMemoryInfo='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('OperatingSystem='+CallBufferToString(@One_GetOperatingSystem));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('OperatingSystem='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('VideoCardName='+CallBufferToString(@One_GetVideoCardName));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('VideoCardName='+ IntToStr(OffsetTick));

  BeginTick := GetTickCount;
  self.Memo1.Lines.Add ('SoundCardName='+CallBufferToString(@One_GetSoundCardName));
  OffsetTick := GetTickCount - BeginTick;
  self.Memo2.Lines.Add ('SoundCardName='+ IntToStr(OffsetTick));

  OffsetTick := GetTickCount - StartTick;
  self.Memo2.Lines.Add ('Total time ='+ IntToStr(OffsetTick));
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  self.Memo1.Clear;
end;

end.
