{===============================================================================}
{    GBIFReporter - Creates statistical reports from GBIF text delimited data   }
{                   (C) 2016-2021 by Mauro J. Cavalcanti                        }
{                         <maurobio@gmail.com>                                  }
{                                                                               }
{  This program is free software; you can redistribute it and/or modify         }
{  it under the terms of the GNU General Public License as published by         }
{  the Free Software Foundation; either version 3 of the License, or            }
{  (at your option) any later version.                                          }
{                                                                               }
{  This program is distributed in the hope that it will be useful,              }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of               }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                }
{  GNU General Public License for more details.                                 }
{                                                                               }
{  You should have received a copy of the GNU General Public License            }
{  along with this program. If not, see <http://www.gnu.org/licenses/>.         }
{                                                                               }
{  Requirements:                                                                }
{     Free Pascal Compiler 3.0+ (www.freepascal.org)                            }
{     Lazarus IDE 2.0+ (www.lazarus.freepascal.org)                             }
{     ZMSQL 0.1.20+ (wiki.freepascal.org/ZMSQL)                                 }
{     HtmlViewer 11.9+ (wiki.freepascal.org/THtmlPort)                          }
{                                                                               }
{  REVISION HISTORY:                                                            }
{     Version 1.00, 5th Dec 21 - Initial release                                }
{     Version 1.10, 7th Dec 21 - Added report menu                              }
{===============================================================================}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ZMConnection, ZMQueryDataSet, HtmlView;

type

  { TMainForm }

  TMainForm = class(TForm)
    HtmlViewer: THtmlViewer;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileOpenItem: TMenuItem;
    FileExitItem: TMenuItem;
    ReportSpeciesItem: TMenuItem;
    ReportGenusItem: TMenuItem;
    ReportFamilyItem: TMenuItem;
    ReportOrderItem: TMenuItem;
    ReportClassItem: TMenuItem;
    ReportPhylumItem: TMenuItem;
    ReportKingdomItem: TMenuItem;
    ReportMenu: TMenuItem;
    S1: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    ZMConnection: TZMConnection;
    ZMQueryDataSet: TZMQueryDataSet;
    procedure FileExitItemClick(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
  private

  public
    outfile: TextFile;
    procedure GenerateReport(Taxon: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function GetFileNameWithoutExt(Filenametouse: string): string;
begin
  Result := ExtractFilename(Copy(Filenametouse, 1,
    RPos(ExtractFileExt(Filenametouse), Filenametouse) - 1));
end;

{ TMainForm }

procedure TMainForm.GenerateReport(Taxon: string);
begin
  Screen.Cursor := crHourGlass;
  ZMQueryDataset.Open;
  ZMQueryDataset.SQL.Text :=
    'SELECT ' + Taxon + ', COUNT(*) FROM ' + ZMQueryDataset.TableName +
    ' GROUP BY ' + Taxon + ' ORDER BY ' + Taxon;
  ZMQueryDataset.QueryExecute;
  WriteLn(outfile, '<table border=1 cellspacing=1 cellpadding=1 width="50%">');
  WriteLn(outfile, '<tr>');
  WriteLn(outfile, '<th>', UpperCase(Taxon), '</th>');
  WriteLn(outfile, '<th>COUNT</th>');
  WriteLn(outfile, '</tr>');
  ZMQueryDataset.First;
  while not ZMQueryDataset.EOF do
  begin
    WriteLn(outfile, '<tr>');
    WriteLn(outfile, '<td align="Left">' +
      ZMQueryDataset.Fields[0].AsString + '</td>');
    WriteLn(outfile, '<td align="Right">' +
      ZMQueryDataset.Fields[1].AsString + '</td>');
    WriteLn(outfile, '</tr>');
    ZMQueryDataset.Next;
  end;
  ZMQueryDataset.Close;
  WriteLn(outfile, '</table>');
  WriteLn(outfile, '<br><br>');
  Screen.Cursor := crDefault;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileOpenItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    ZMConnection.DatabasePath := GetCurrentDir;
    ZMConnection.Connect;
    ZMQueryDataSet.TableName := GetFileNameWithoutExt(OpenDialog.FileName);
    ZMQueryDataSet.LoadFromTable;
    Screen.Cursor := crDefault;
    ShowMessage('Read ' + IntToStr(ZMQueryDataset.RecordCount) + ' record(s)');
    AssignFile(outfile, 'report.htm');
    Rewrite(outfile);
    WriteLn(outfile, '<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 3.2//EN"">');
    WriteLn(outfile, '<html>');
    WriteLn(outfile, '<head>');
    WriteLn(outfile, '<title>Statistical report for ',
      ExtractFileName(OpenDialog.FileName), '</title>');
    WriteLn(outfile, '</head>');
    WriteLn(outfile, '<body>');
    WriteLn(outfile, '<h3>Statistical report for "',
      ExtractFileName(OpenDialog.FileName), '"</h3>');

    if ReportKingdomItem.Checked then
      GenerateReport('kingdom');
    if ReportPhylumItem.Checked then
      GenerateReport('phylum');
    if ReportClassItem.Checked then
      GenerateReport('class');
    if ReportOrderItem.Checked then
      GenerateReport('order');
    if ReportFamilyItem.Checked then
      GenerateReport('family');
    if ReportGenusItem.Checked then
      GenerateReport('genus');
    if ReportSpeciesItem.Checked then
      GenerateReport('species');

    WriteLn(outfile, '</body>');
    WriteLn(outfile, '</html>');
    ZMConnection.Disconnect;
    CloseFile(outfile);
    HtmlViewer.LoadFromFile('report.htm');
    HtmlViewer.Visible := True;
  end;
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  MessageDlg('Information', 'Creates statistical reports from GBIF text delimited data' +
    LineEnding + '(c) 2016-2021 Mauro J. Cavalcanti' + LineEnding +
    'Ecoinformatics Studio, Rio de Janeiro, Brazil' + LineEnding +
    'E-mail: maurobio@gmail.com', mtInformation, [mbOK], 0);
end;

end.
