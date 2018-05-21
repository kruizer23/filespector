program filespector;
//***************************************************************************************
//  Description: Contains the main program entry.
//    File Name: filespector.lpr
//
//---------------------------------------------------------------------------------------
//                          C O P Y R I G H T
//---------------------------------------------------------------------------------------
//           Copyright (c) 2018 by Frank Voorburg   All rights reserved
//
//   This software has been carefully tested, but is not guaranteed for any particular
// purpose. The author does not offer any warranties and does not guarantee the accuracy,
//   adequacy, or completeness of the software and is not responsible for any errors or
//              omissions or the results obtained from use of the software.
//
//---------------------------------------------------------------------------------------
//                            L I C E N S E
//---------------------------------------------------------------------------------------
// This file is part of FileSpector. FileSpector is free software: you can redistribute
// it and/or modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// FileSpector is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this
// program.  If not, see <http://www.gnu.org/licenses/>.
//
//***************************************************************************************
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

//***************************************************************************************
// Includes
//***************************************************************************************
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, SearchSettings, CommandRunner, FileContentSearcher,
  TextEditor, VersionInfo, AboutUnit
  { you can add units after this }, SysUtils, LCLTranslator;

{$R *.res}

//***************************************************************************************
// NAME:           WriteHelp
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Displays the application's help information on the standard output.
//
//***************************************************************************************
procedure WriteHelp;
begin
  WriteLn('Usage: filespector [OPTIONS]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  -?             --help                    Show help options');
  WriteLn('  -a             --autostart               Start search after opening');
  WriteLn('  -d [dir]       --directory[=dir]         Directory to search in');
  WriteLn('  -i [off|on]    --ignore-case[=off|on]    Ignore case during search');
  WriteLn('  -l [code]      --lang [code]             Set user interface language');
  WriteLn('  -p [pattern]   --pattern[=pattern]       File pattern to match');
  WriteLn('  -r [off|on]    --recursive[=off|on]      Recurse into directories');
  WriteLn('  -s [text]      --searchterm[=text]       Text to find in files');
  WriteLn('');
  WriteLn('FileSpector is a GUI tool for quickly finding all text occurrences in multiple');
  WriteLn('files in a directory, matching a specific file extension pattern.');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  filespector -i on -r off -d /home/user -p "*.txt|*.log" -s "text to find"');
  WriteLn('  filespector --lang de --directory="/home/user/my files" --searchterm=findme');
  WriteLn('');
end; //*** end of WriteHelp ***


//***************************************************************************************
// NAME:           ProcessCommandLine
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Checks and extracts all relevant command line options.
//
//***************************************************************************************
procedure ProcessCommandLine;
var
  checkResult: String;
  optionValue: String;
begin
  // Display help information if requested.
  if Application.HasOption('?', 'help') then
  begin
    // Display application's help information.
    WriteHelp;
    // Stop the program.
    halt;
  end;
  // Initialize command line option related global vars.
  CmdLineIgnoreCaseOptionFound := False;
  CmdLineIgnoreCaseOption := False;
  CmdLineRecursiveOptionFound := False;
  CmdLineRecursiveOption := False;
  CmdLineDirectoryOption := '';
  CmdLineAutoStartOption := False;
  CmdLineFilePatternOption := '';
  CmdLineSearchTextOption := '';

  // Check the supported command line options.
  checkResult := Application.CheckOptions('?ad:i:l:p:r:s:',
        'help autostart directory: ignore-case: lang: pattern: recursive: searchterm:');
  // Only extract the command line options if valid ones were specified.
  if checkResult = '' then
  begin
    // Extract autostart option.
    if Application.HasOption('a', 'autostart') then
    begin
      CmdLineAutoStartOption := True;
    end;
    // Extract ignore-case option.
    if Application.HasOption('i', 'ignore-case') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('i', 'ignore-case');
      if optionValue = 'off' then
      begin
        CmdLineIgnoreCaseOptionFound := True;
        CmdLineIgnoreCaseOption := False;
      end
      else if optionValue = 'on' then
      begin
        CmdLineIgnoreCaseOptionFound := True;
        CmdLineIgnoreCaseOption := True;
      end;
    end;
    // Extract recursive search option.
    if Application.HasOption('r', 'recursive') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('r', 'recursive');
      if optionValue = 'off' then
      begin
        CmdLineRecursiveOptionFound := True;
        CmdLineRecursiveOption := False;
      end
      else if optionValue = 'on' then
      begin
        CmdLineRecursiveOptionFound := True;
        CmdLineRecursiveOption := True;
      end;
    end;
    // Extract the initial search directory.
    if Application.HasOption('d', 'directory') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('d', 'directory');
      // Clean up.
      Trim(optionValue);
      if optionValue <> '' then
      begin
        // A directory with spaces is specified with double-quotes around it. Remove
        // these here again.
        StringReplace(optionValue, '"', '', [rfReplaceAll]);
        // Store the option value.
        CmdLineDirectoryOption := optionValue;
      end;
    end;
    // Extract the file pattern.
    if Application.HasOption('p', 'pattern') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('p', 'pattern');
      // Clean up.
      Trim(optionValue);
      if optionValue <> '' then
      begin
        // Multiple file pattern are seperated with a |. For this to work, the option
        // value needs to be specified with double-quotes around it. Remove these here
        // again.
        StringReplace(optionValue, '"', '', [rfReplaceAll]);
        // Store the option value.
        CmdLineFilePatternOption := optionValue;
      end;
    end;
    // Extract the search term.
    if Application.HasOption('s', 'searchterm') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('s', 'searchterm');
      // Clean up.
      Trim(optionValue);
      if optionValue <> '' then
      begin
        // A search term with spaces is specified with double-quotes around it. Remove
        // these here again.
        StringReplace(optionValue, '"', '', [rfReplaceAll]);
        // Store the option value.
        CmdLineSearchTextOption := optionValue;
      end;
    end;
  end;
end; //*** end of ProcessCommandLine ***


//***************************************************************************************
// NAME:           Main
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Program entry point.
//
//***************************************************************************************
begin
  // Set flag to specify that every form must have a resource.
  RequireDerivedFormResource:=True;
  // Set default language and enable localization support.
  SetDefaultLang('', '');
  // Initialize the application.
  Application.Initialize;
  // Process the command line.
  ProcessCommandLine;
  Application.CreateForm(TMainForm, MainForm);
  // Start running the application.
  Application.Run;
end.
//******************************** end of filespector.lpr *******************************

