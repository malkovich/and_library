unit DLLExports; 

interface 

Uses Windows, Classes, Sysutils, imagehlp ; 

type 
  TDLLExportCallback = function (Sender: Pointer; const name: String; ordinal: Integer; address: Pointer): Boolean;
  { Note: address is a RVA here, not a usable virtual address! } 
  DLLToolsError = Class( Exception );

Procedure ListDLLExports( const filename: String; callback: TDLLExportCallback; Sender: Pointer);
Procedure DumpExportDirectory( Const ExportDirectory: TImageExportDirectory; lines: TStrings; const Image: LoadedImage ); 
Function RVAToPchar( rva: DWORD; const Image: LoadedImage ): PChar; 
Function RVAToPointer( rva: DWORD; const Image: LoadedImage ): Pointer;

implementation 

resourcestring 
eDLLNotFound = 'ListDLLExports: DLL %s does not exist!';

{+---------------------------------------------------------------------- 
| Procedure EnumExports 
| 
| Parameters :
| ExportDirectory: IMAGE_EXPORT_DIRECTORY record to enumerate 
| image : LOADED_IMAGE record for the DLL the export directory belongs 
| to. 
| callback : callback function to hand the found exports to, must not be 
Nil 
| Description: 
| The export directory of a PE image contains three RVAs that point at
tables 
| which describe the exported functions. The first is an array of RVAs 
that 
| refer to the exported function names, these we translate to PChars to 
| get the exported name. The second array is an array of Word that 
contains 
| the export ordinal for the matching entry in the names array. The 
ordinal 
| is biased, that is we have to add the ExportDirectory.Base value to it 
to 
| get the actual export ordinal. The biased ordinal serves as index for 
the 
| third array, which is an array of RVAs that give the position of the 
| function code in the image. We don't translate these RVAs since the DLL 
| is not relocated since we load it via MapAndLoad. The function array is 
| usually much larger than the names array, since the ordinals for the 
| exported functions do not have to be in sequence, there can be (and 
| frequently are) gaps in the sequence, for which the matching entries in 
the 
| function RVA array are garbage. 
| Error Conditions: none 
| Created: 9.1.2000 by P. Below 
+----------------------------------------------------------------------} 
Procedure EnumExports( const ExportDirectory : TImageExportDirectory ; 
    const image : LoadedImage ; callback : TDLLExportCallback; Sender: Pointer) ;
Type 
  TDWordArray = Array [0..$FFFFF] of DWORD;
Var 
  i: Cardinal;
  pNameRVAs, pFunctionRVas: ^TDWordArray;
  pOrdinals: ^TWordArray;
  name: String;
  address: Pointer;
  ordinal: Word;
Begin { EnumExports } 
  pNameRVAs := RVAToPointer( DWORD(ExportDirectory.AddressOfNames), image );
  pFunctionRVAs := RVAToPointer( DWORD(ExportDirectory.AddressOfFunctions), image );
  pOrdinals := RVAToPointer( DWORD(ExportDirectory.AddressOfNameOrdinals), image );
  For i:= 0 to Pred( ExportDirectory.NumberOfNames ) Do
  Begin
    name := RVAToPChar( pNameRVAs^[i], image );
    ordinal := pOrdinals^[i];
    address := Pointer( pFunctionRVAs^[ ordinal ] );
    If not callback(Sender, name, ordinal+ExportDirectory.Base, address ) Then
    Exit;
  End; { For }
End; { EnumExports } 

{+----------------------------------------------------------------------
| Procedure ListDLLExports 
| 
| Parameters : 
| filename : full pathname of DLL to examine 
| callback : callback to hand the found exports to, must not be Nil 
| Description: 
| Loads the passed DLL using the LoadImage function, finds the exported 
| names table and reads it. Each found entry is handed to the callback 
| for further processing, until no more entries remain or the callback 
| returns false. Note that the address passed to the callback for a 
exported 
| function is an RVA, so not identical to the address the function would 
| have in a properly loaded and relocated DLL! 
| Error Conditions: 
| Exceptions are raised if 
| - the passed DLL does not exist or could not be loaded 
| - no callback was passed (only if assertions are on) 
| - an API function failed 
| Created: 9.1.2000 by P. Below 
+----------------------------------------------------------------------} 
Procedure ListDLLExports( const filename : String ; callback :TDLLExportCallback; Sender: Pointer) ;
Var 
  imageinfo: LoadedImage;
  pExportDirectory: PImageExportDirectory;
  dirsize: Cardinal;
Begin { ListDLLExports }
  Assert( Assigned( callback ));
  If not FileExists( filename ) Then
    raise DLLToolsError.CreateFmt( eDLLnotFound, [filename] );

  If MapAndLoad( PChar( filename ), nil, @imageinfo, true, true ) Then
  try 
    pExportDirectory :=
    ImageDirectoryEntryToData(
    imageinfo.MappedAddress, false,
    IMAGE_DIRECTORY_ENTRY_EXPORT, dirsize );

    If pExportDirectory = Nil Then
      RaiseLastOSError
    Else
    EnumExports( pExportDirectory^, imageinfo, callback, Sender);
  finally
    UnMapAndLoad( @imageinfo );
  end
  Else
    RaiseLastOSError;
End; { ListDLLExports } 

{+---------------------------------------------------------------------- 
| Procedure DumpExportDirectory 
| 
| Parameters : 
| ExportDirectory: a IMAGE_EXPORT_DIRECTORY record 
| lines : a TStrings descendend to put the info into, must not be Nil 
| Description: 
| Dumps the fields of the passed structure to the passed strings 
descendent 
| as strings. 
| Error Conditions: 
| will raise an exception if lines is Nil and assertions are enabled. 
| Created: 9.1.2000 by P. Below 
+----------------------------------------------------------------------} 
Procedure DumpExportDirectory( Const ExportDirectory : TImageExportDirectory; 
lines : TStrings; const Image: LoadedImage ) ; 
Begin { DumpExportDirectory } 
Assert( Assigned( lines )); 

lines.add( 'Dump of IMAGE_EXPORT_DIRECTORY' ); 
lines.add( format('Characteristics: %d', 
[ExportDirectory.Characteristics])); 
lines.add( format('TimeDateStamp: %d', 
[ExportDirectory.TimeDateStamp])); 
lines.add( format('Version: %d.%d', 
[ExportDirectory.MajorVersion, 
ExportDirectory.MinorVersion])); 
lines.add( format('Name (RVA): %x', 
[ExportDirectory.Name])); 
lines.add( format('Name (translated): %s', 
[RVAToPchar( ExportDirectory.name, Image )])); 
lines.add( format('Base: %d', 
[ExportDirectory.Base])); 
lines.add( format('NumberOfFunctions: %d', 
[ExportDirectory.NumberOfFunctions])); 
lines.add( format('NumberOfNames: %d', 
[ExportDirectory.NumberOfNames])); 
lines.add( format('AddressOfFunctions (RVA): %p', 
[Pointer(ExportDirectory.AddressOfFunctions)])); 
lines.add( format('AddressOfNames (RVA): %p', 
[Pointer(ExportDirectory.AddressOfNames)])); 
lines.add( format('AddressOfNameOrdinals (RVA): %p', 
[Pointer(ExportDirectory.AddressOfNameOrdinals)])); 
End; { DumpExportDirectory } 

{+---------------------------------------------------------------------- 
| Function RVAToPointer 
| 
| Parameters : 
| rva : a relative virtual address to translate 
| Image : LOADED_IMAGE structure for the image the RVA relates to 
| Returns : translated address 
| Description: 
| Uses the ImageRVAToVA function to translate the RVA to a virtual 
| address. 
| Error Conditions: 
| Will raise an exception if the translation failed 
| Created: 9.1.2000 by P. Below 
+----------------------------------------------------------------------} 
Function RVAToPointer( rva : DWORD ; const Image : LoadedImage ) : Pointer; 
var 
pDummy: PImageSectionHeader; 
Begin { RVAToPchar } 
pDummy := nil; 
Result := 
ImageRvaToVa( Image.FileHeader, Image.MappedAddress, rva, 
pDummy ); 
If Result = Nil Then 
RaiseLastOSError; 
End; { RVAToPointer } 

{+---------------------------------------------------------------------- 
| Function RVAToPchar 
| 
| Parameters : 
| rva : a relative virtual address to translate 
| Image : LOADED_IMAGE structure for the image the RVA relates to 
| Returns : translated address 
| Description: 
| Uses the RVAToPointer function to translate the RVA to a virtual 
| address. Note that we do not check that the address does indeed point 
| to a zero-terminated string! 
| Error Conditions: 
| Will raise an exception if the translation failed 
| Created: 9.1.2000 by P. Below 
+----------------------------------------------------------------------} 
Function RVAToPchar( rva : DWORD ; const Image : LoadedImage ) : PChar ; 
Begin { RVAToPchar } 
Result := RVAToPointer( rva, image ); 
End; { RVAToPchar } 

end.