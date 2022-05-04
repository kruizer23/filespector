unit CustomUtil;
//***************************************************************************************
//  Description: FileSpector specific utilities
//    File Name: customutil.pas
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

interface
//***************************************************************************************
// Includes
//***************************************************************************************
uses
  Classes, SysUtils, Fgl, StrUtils;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ THighlightSplit --------------------------------------
  THighlightSplit = class (TObject)
  private
    type
      //------------------------------ TNode --------------------------------------------
      TNode = class (TObject)
      private
        FText: String;         // Substring of the node.
        FHighlight: Boolean;   // True if the substring should be highlighted.
      public
        property Text: String read FText write FText;
        property Highlight: Boolean read FHighlight write FHighlight;
      end;
      //------------------------------ TSplits ------------------------------------------
      TSplits = specialize TFPGObjectList<TNode>;
  private
    FSplits: TSplits;
    FText: String;
    FHighlight: String;
    procedure DoSplit;
    function GetCount: Integer;
    function GetNode(Index: Integer): TNode;
  public
    constructor Create(AText: String; AHighlight: String);
    destructor  Destroy; override;
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: TNode read GetNode; default;
  end;


implementation
//---------------------------------------------------------------------------------------
//-------------------------------- THighlightSplit ---------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      AText The text to split based on the specified AHighlight.
//                 AHighlight Highlight text to use as the split delimiter.
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor THighlightSplit.Create(AText: string; AHighlight: string);
begin
  // Call inherited constructor.
  inherited Create;

  // Check parameters
  Assert(AText <> '');
  Assert(AHighlight <> '');
  Assert(Length(AHighlight) <= Length(AText));

  // Set the fields.
  FText := AText;
  FHighlight := AHighlight;
  // Create the splits list.
  FSplits := TSplits.Create;
  // Perform the split operation.
  DoSplit;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class destructor.
//
//***************************************************************************************
destructor THighlightSplit.Destroy;
begin
  // Release the splits list.
  FSplits.Free;
  // call inherited destructor
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           DoSplit
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Splits the string into substring nodes. Each node holds a boolean
//                 flag to specify if that substring should be highlighted.
//
//***************************************************************************************
procedure THighlightSplit.DoSplit;
var
  Node: TNode;
  UpperText: String;
  UpperHighlight: String;
  CurrentHighlightPos: Integer = 1;
  LastCopiedPos: Integer = 0;
  SplitStr: String;
begin
  // Copy the text and highlight strings in their uppercase version to be able to search
  // for the highlight substring case insensitive.
  UpperText := UpperCase(FText);
  UpperHighlight := UpperCase(FHighlight);

  // Enter splitting loop.
  repeat
    // Locate start of the highlight substring.
    CurrentHighlightPos := PosEx(UpperHighlight, UpperText, CurrentHighlightPos);
    // Highlight substring found?
    if CurrentHighlightPos <> 0 then
    begin
      // Any non highlight substring before the current highlight that still needs
      // to be copied?
      if CurrentHighlightPos > (LastCopiedPos + 1) then
      begin
        // Create the node.
        SplitStr := Copy(FText, LastCopiedPos + 1, CurrentHighlightPos - LastCopiedPos - 1);
        Node := TNode.Create;
        Node.Text := SplitStr;
        Node.Highlight := False;
        FSplits.Add(Node);
        // Substring processed, so update the variable.
        LastCopiedPos := CurrentHighlightPos - 1;
      end;

      // Create the node.
      SplitStr := Copy(FText, CurrentHighlightPos, Length(FHighlight));
      Node := TNode.Create;
      Node.Text := SplitStr;
      Node.Highlight := True;
      FSplits.Add(Node);
      // Move forward until after the highlight.
      Inc(CurrentHighlightPos, Length(FHighlight));
      // Store the position of the last part that was copied.
      LastCopiedPos := CurrentHighlightPos - 1;
    end;
  until CurrentHighlightPos = 0;

  // Still a remainder substring left to be processed?
  if LastCopiedPos < Length(FText) then
  begin
    // Create the node.
    SplitStr := Copy(FText, LastCopiedPos + 1, Length(FText) - LastCopiedPos);
    Node := TNode.Create;
    Node.Text := SplitStr;
    Node.Highlight := False;
    FSplits.Add(Node);
  end;
end; //*** end of DoSplit ****


//***************************************************************************************
// NAME:           GetCount
// PARAMETER:      none
// RETURN VALUE:   Total count of splits.
// DESCRIPTION:    Getter for the total split count.
//
//***************************************************************************************
function THighlightSplit.GetCount: Integer;
begin
  Result := FSplits.Count;
end; //*** end of GetCount ***


//***************************************************************************************
// NAME:           GetNode
// PARAMETER:      Index Index of the node to get.
// RETURN VALUE:   The node as the specified index.
// DESCRIPTION:    Getter for obtaining a node.
//
//***************************************************************************************
function THighlightSplit.GetNode(Index: Integer): TNode;
begin
  // Initialize the result.
  Result := nil;

  // Verify parameter.
  Assert(Index < GetCount);

  // Only continue if the index is valid.
  if Index < GetCount then
  begin
    Result := FSplits[Index];
  end;
end; //*** end of GetNode ***


end.
//******************************** end of customutil.pas ********************************

