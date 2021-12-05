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

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileOpenItemClick(Sender: TObject);
var
  outfile: TextFile;
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    ZMConnection.DatabasePath := GetCurrentDir;
    ZMConnection.Connect;
    ZMQueryDataSet.TableName := GetFileNameWithoutExt(OpenDialog.FileName);
    ZMQueryDataSet.LoadFromTable;
    Screen.Cursor := crDefault;
    //ShowMessage(IntToStr(ZMQueryDataset.RecordCount) + ' record(s)');

    AssignFile(Outfile, 'report.htm');
    Rewrite(Outfile);
    WriteLn(outfile, '<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 3.2//EN"">');
    WriteLn(outfile, '<html>');
    WriteLn(outfile, '<head>');
    WriteLn(outfile, '<title>Statistical report for ',
      ExtractFileName(OpenDialog.FileName), '</title>');
    WriteLn(outfile, '</head>');
    WriteLn(outfile, '<body>');
    WriteLn(outfile, '<h3>Statistical report for "',
      ExtractFileName(OpenDialog.FileName), '"</h3>');

    Screen.Cursor := crHourGlass;
    ZMQueryDataset.SQL.Text :=
      'SELECT kingdom, COUNT(*) FROM ' + ZMQueryDataset.TableName
      //+ ' WHERE Length(kingdom) > 0 AND Length(species) > 0 AND basisOfRecord <> "FOSSIL_SPECIMEN"'
      + ' GROUP BY kingdom ORDER BY kingdom';
    Screen.Cursor := crHourGlass;ZMQueryDataset.QueryExecute;
    WriteLn(outfile, '<table border=1 cellspacing=1 cellpadding=1 width="50%">');
    WriteLn(outfile, '<tr>');
    WriteLn(outfile, '<th>KINGDOM</th>');
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

    ZMQueryDataset.Open;
    ZMQueryDataset.SQL.Text :=
      'SELECT phylum, COUNT(*) FROM ' + ZMQueryDataset.TableName
      + ' GROUP BY phylum ORDER BY phylum';
    ZMQueryDataset.QueryExecute;
    WriteLn(outfile, '<table border=1 cellspacing=1 cellpadding=1 width="50%">');
    WriteLn(outfile, '<tr>');
    WriteLn(outfile, '<th>PHYLUM</th>');
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

    ZMQueryDataset.Open;
    ZMQueryDataset.SQL.Text :=
      'SELECT class, COUNT(*) FROM ' + ZMQueryDataset.TableName
      + ' GROUP BY class ORDER BY class';
    ZMQueryDataset.QueryExecute;
    WriteLn(outfile, '<table border=1 cellspacing=1 cellpadding=1 width="50%">');
    WriteLn(outfile, '<tr>');
    WriteLn(outfile, '<th>CLASS</th>');
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

    ZMQueryDataset.Open;
    ZMQueryDataset.SQL.Text :=
      'SELECT order, COUNT(*) FROM ' + ZMQueryDataset.TableName
      + ' GROUP BY order ORDER BY order';
    ZMQueryDataset.QueryExecute;
    WriteLn(outfile, '<table border=1 cellspacing=1 cellpadding=1 width="50%">');
    WriteLn(outfile, '<tr>');
    WriteLn(outfile, '<th>ORDER</th>');
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
    WriteLn(outfile, '</table>');
    WriteLn(outfile, '</body>');
    WriteLn(outfile, '</html>');
    ZMConnection.Disconnect;
    CloseFile(Outfile);
    HtmlViewer.LoadFromFile('report.htm');
    HtmlViewer.Visible := True;
    Screen.Cursor := crDefault;
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
