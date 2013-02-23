unit RGBUnit;

interface
uses   windows,Graphics,comctrls;
const
    MaxPixelCount       =     32768;
type   
    pRGBArray     =     ^TRGBArray;
    TRGBArray     =     ARRAY[0..MaxPixelCount-1]   OF   TRGBTriple;
    function   Min(a,   b:   integer):   integer;   
    function   Max(a,   b:   integer):   integer;   
    

    procedure   RGBChange(SrcBmp,DestBmp:Tbitmap;RedChange,GreenChange, BlueChange:integer); overload;
    procedure   RGBChange(SrcBmp:Tbitmap;RedChange,GreenChange,BlueChange:integer; AProgressBar:TProgressBar); overload;

    procedure   BrightnessChange(SrcBmp,DestBmp:Tbitmap;ValueChange:integer); overload;
    procedure   BrightnessChange(SrcBmp:Tbitmap;ValueChange:integer; AProgressBar:TProgressBar); overload;
    procedure   ContrastChange(SrcBmp,DestBmp:Tbitmap;ValueChange:integer);   overload;   
    procedure   ContrastChange(SrcBmp:Tbitmap;ValueChange:integer; AProgressBar:TProgressBar); overload;

implementation

function   Min(a,   b:   integer):   integer;
begin   
    if   a   <   b   then   result   :=   a
    else   result   :=   b;   
end;   
    
function   Max(a,   b:   integer):   integer;
begin   
    if   a   >   b   then   result   :=   a   
    else   result   :=   b;   
end;
    
//rgb色彩   
procedure   RGBChange(SrcBmp,DestBmp:Tbitmap;RedChange,GreenChange,BlueChange:integer);   
var
  //   R,G,B:integer;   
    SrcRow,   DestRow:pRGBArray;   
    i,j:integer;   
begin
    for   i   :=   0   to   SrcBmp.Height-   1   do   
    begin   
        SrcRow   :=   SrcBmp.ScanLine[i];   
        DestRow   :=DestBmp.ScanLine[i];
    
        for   j   :=   0   to   SrcBmp.Width   -   1   do   
        begin   
                //   add   brightness   value   to   pixel's   RGB   values
            if   RedChange>   0   then   
                DestRow[j].rgbtRed   :=   Min(255,   SrcRow[j].rgbtRed   +   RedChange)   
            else   
                DestRow[j].rgbtRed   :=   Max(0,   SrcRow[j].rgbtRed   +   RedChange);
    
            if   GreenChange>   0   then   
                DestRow[j].rgbtGreen   :=   Min(255,   SrcRow[j].rgbtGreen   +GreenChange)   
            else
                DestRow[j].rgbtGreen   :=   Max(0,   SrcRow[j].rgbtGreen   +GreenChange);   
    
            if   BlueChange>   0   then   
                DestRow[j].rgbtBlue   :=   Min(255,   SrcRow[j].rgbtBlue   +BlueChange)
            else   
                DestRow[j].rgbtBlue   :=   Max(0,   SrcRow[j].rgbtBlue   +BlueChange);   
    
        end;
    end;   
end;   
    
//rgb色彩
procedure   RGBChange(SrcBmp:Tbitmap;RedChange,GreenChange,BlueChange:integer;AProgressBar:TProgressBar);     overload;   
var   
  //   R,G,B:integer;   
    SrcRow   :pRGBArray;
    i,j:integer;   
begin   
    AProgressBar.Max:=SrcBmp.Height-   1;   
    for   i   :=   0   to   SrcBmp.Height-   1   do
    begin   
        SrcRow   :=   SrcBmp.ScanLine[i];   
    //     DestRow   :=DestBmp.ScanLine[i];   

        for   j   :=   0   to   SrcBmp.Width   -   1   do   
        begin   
                //   add   brightness   value   to   pixel's   RGB   values   
            if   RedChange>   0   then
                SrcRow[j].rgbtRed   :=   Min(255,   SrcRow[j].rgbtRed   +   RedChange)   
            else   
                SrcRow[j].rgbtRed   :=   Max(0,   SrcRow[j].rgbtRed   +   RedChange);   

            if   GreenChange>   0   then   
                SrcRow[j].rgbtGreen   :=   Min(255,   SrcRow[j].rgbtGreen   +GreenChange)   
            else   
                SrcRow[j].rgbtGreen   :=   Max(0,   SrcRow[j].rgbtGreen   +GreenChange);
    
            if   BlueChange>   0   then   
                SrcRow[j].rgbtBlue   :=   Min(255,   SrcRow[j].rgbtBlue   +BlueChange)   
            else
                SrcRow[j].rgbtBlue   :=   Max(0,   SrcRow[j].rgbtBlue   +BlueChange);   
    
        end;   
        AProgressBar.Position:=i;
    end;   
    
end;

//亮度   
  procedure   BrightnessChange(SrcBmp,DestBmp:Tbitmap;ValueChange:integer);   
  var   
      i,   j:   integer;   
      SrcRow,   DestRow:   pRGBArray;   
  begin   
          //   get   brightness   increment   value   
    
          //   for   each   row   of   pixels   
      for   i   :=   0   to   SrcBmp.Height   -   1   do   
      begin   
          SrcRow   :=   SrcBmp.ScanLine[i];   
          DestRow   :=   DestBmp.ScanLine[i];   
    
              //   for   each   pixel   in   row   
          for   j   :=   0   to   SrcBmp.Width   -   1   do   
          begin   
                  //   add   brightness   value   to   pixel's   RGB   values   
              if   ValueChange   >   0   then   
              begin   
                      //   RGB   values   must   be   less   than   256   
                  DestRow[j].rgbtRed   :=   Min(255,   SrcRow[j].rgbtRed   +ValueChange);   
                  DestRow[j].rgbtGreen   :=   Min(255,   SrcRow[j].rgbtGreen   +   ValueChange);   
                  DestRow[j].rgbtBlue   :=   Min(255,   SrcRow[j].rgbtBlue   +ValueChange);   
              end   else   begin   
                      //   RGB   values   must   be   greater   or   equal   than   0   
                  DestRow[j].rgbtRed   :=   Max(0,   SrcRow[j].rgbtRed   +ValueChange);   
                  DestRow[j].rgbtGreen   :=   Max(0,   SrcRow[j].rgbtGreen   +ValueChange);   
                  DestRow[j].rgbtBlue   :=   Max(0,   SrcRow[j].rgbtBlue   +ValueChange);   
              end;   
          end;   
      end;   
  end;   
    
    
  //亮度   
  procedure   BrightnessChange(SrcBmp:Tbitmap;ValueChange:integer;AProgressBar:TProgressBar);   
  var   
      i,   j:   integer;   
      SrcRow:   pRGBArray;   
  begin   
      AProgressBar.Max:=SrcBmp.Height-   1;   
      for   i   :=   0   to   SrcBmp.Height   -   1   do   
      begin   
          SrcRow   :=   SrcBmp.ScanLine[i];   
          //DestRow   :=   DestBmp.ScanLine[i];   
    
              //   for   each   pixel   in   row   
          for   j   :=   0   to   SrcBmp.Width   -   1   do   
          begin   
                  //   add   brightness   value   to   pixel's   RGB   values   
              if   ValueChange   >   0   then   
              begin   
                      //   RGB   values   must   be   less   than   256   
                  SrcRow[j].rgbtRed   :=   Min(255,   SrcRow[j].rgbtRed   +ValueChange);   
                  SrcRow[j].rgbtGreen   :=   Min(255,   SrcRow[j].rgbtGreen   +   ValueChange);   
                  SrcRow[j].rgbtBlue   :=   Min(255,   SrcRow[j].rgbtBlue   +ValueChange);   
              end   else   begin   
                      //   RGB   values   must   be   greater   or   equal   than   0   
                  SrcRow[j].rgbtRed   :=   Max(0,   SrcRow[j].rgbtRed   +ValueChange);   
                  SrcRow[j].rgbtGreen   :=   Max(0,   SrcRow[j].rgbtGreen   +ValueChange);   
                  SrcRow[j].rgbtBlue   :=   Max(0,   SrcRow[j].rgbtBlue   +ValueChange);   
              end;   
          end;   
          AProgressBar.Position:=i;   
      end;   
    
  end;
//对比度   
procedure   ContrastChange(SrcBmp,DestBmp:Tbitmap;ValueChange:integer);
var   
    X,   Y:   Integer;   
    SrcRow,DestRow:   pRGBArray;   
begin   
    if   valuechange>=0
        then   
            begin   
                    
                //AProgressBar.Max:=SrcBmp.Height   -   1;   
                for   Y   :=   0   to   SrcBmp.Height   -   1   do
                begin   
                    SrcRow   :=   SrcBmp.ScanLine[Y];   
                    DestRow:=DestBmp.ScanLine[y];   
                    for   X   :=   0   to   SrcBmp.Width   -   1   do   
                    begin
                        if   SrcRow[x].rgbtRed>=128   then     DestRow[x].rgbtRed:=min(255,SrcRow[x].rgbtRed+ValueChange)   
                        else   
                            DestRow[x].rgbtRed:=max(0,SrcRow[x].rgbtRed-ValueChange);   
    
                        if   SrcRow[x].rgbtGreen>=128   then     DestRow[x].rgbtGreen:=min(255,SrcRow[x].rgbtGreen+ValueChange)
                        else   
                            DestRow[x].rgbtGreen:=max(0,SrcRow[x].rgbtGreen-ValueChange);   
                        if   SrcRow[x].rgbtBlue>=128   then     DestRow[x].rgbtBlue:=min(255,SrcRow[x].rgbtBlue+ValueChange)   
                        else   
                            DestRow[x].rgbtBlue:=max(0,SrcRow[x].rgbtBlue-ValueChange);
    
                    end;   
                  end;   
            end   
        else
            begin   
                //AProgressBar.Position:=0;   
            //AProgressBar.Max:=SrcBmp.Height   -   1;   
    
            for   Y   :=   0   to   SrcBmp.Height   -   1   do
            begin   
                SrcRow   :=   SrcBmp.ScanLine[Y];   
                DestRow:=DestBmp.ScanLine[y];   
                for   X   :=   0   to   SrcBmp.Width   -   1   do   
                begin
                        DestRow[x].rgbtRed:=min(128,SrcRow[x].rgbtRed-ValueChange);   
                        DestRow[x].rgbtGreen:=min(128,SrcRow[x].rgbtGreen-ValueChange);   
                        DestRow[x].rgbtBlue:=min(128,SrcRow[x].rgbtBlue-ValueChange);   
                end;   

            end;   
    
            end;   
end;   

//对比度   
procedure   ContrastChange(SrcBmp:Tbitmap;ValueChange:integer;AProgressBar:TProgressBar);   
var   
    X,   Y:   Integer;   
    SrcRow:   pRGBArray;
begin   
    if   valuechange>=0   
        then   
            begin   

                AProgressBar.Max:=SrcBmp.Height   -   1;   
                for   Y   :=   0   to   SrcBmp.Height   -   1   do   
                begin   
                    SrcRow   :=   SrcBmp.ScanLine[Y];   
                    for   X   :=   0   to   SrcBmp.Width   -   1   do
                        begin   
                            if   SrcRow[x].rgbtRed>=128   then     SrcRow[x].rgbtRed:=min(255,SrcRow[x].rgbtRed+ValueChange)   
                            else   
                                SrcRow[x].rgbtRed:=max(0,SrcRow[x].rgbtRed-ValueChange);   

                            if   SrcRow[x].rgbtGreen>=128   then     SrcRow[x].rgbtGreen:=min(255,SrcRow[x].rgbtGreen+ValueChange)   
                            else   
                                SrcRow[x].rgbtGreen:=max(0,SrcRow[x].rgbtGreen-ValueChange);   
                            if   SrcRow[x].rgbtBlue>=128   then     SrcRow[x].rgbtBlue:=min(255,SrcRow[x].rgbtBlue+ValueChange)   
                            else
                                SrcRow[x].rgbtBlue:=max(0,SrcRow[x].rgbtBlue-ValueChange);   
                        end;   
                end;   
                    AProgressBar.Position:=y;   
            end   
        else   
            begin   
            AProgressBar.Position:=0;   
            AProgressBar.Max:=SrcBmp.Height   -   1;
    
            for   Y   :=   0   to   SrcBmp.Height   -   1   do   
            begin   
                SrcRow   :=   SrcBmp.ScanLine[Y];   
                for   X   :=   0   to   SrcBmp.Width   -   1   do   
                begin
                    SrcRow[x].rgbtRed:=min(128,SrcRow[x].rgbtRed-ValueChange);   
                    SrcRow[x].rgbtGreen:=min(128,SrcRow[x].rgbtGreen-ValueChange);   
                    SrcRow[x].rgbtBlue:=min(128,SrcRow[x].rgbtBlue-ValueChange);   
                end;
                AProgressBar.Position:=y;   
    
            end;   

            end;   
end;
end.
