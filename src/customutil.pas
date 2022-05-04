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
  Classes, SysUtils, Fgl;


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
begin
  // TODO Implement the actual split operation.
  // Test string: 'The fox chased the bunny!', 'the'
  Node := TNode.Create;
  Node.Text := 'The';
  Node.Highlight := True;
  FSplits.Add(Node);

  Node := TNode.Create;
  Node.Text := ' fox chased ';
  Node.Highlight := False;
  FSplits.Add(Node);

  Node := TNode.Create;
  Node.Text := 'the';
  Node.Highlight := True;
  FSplits.Add(Node);

  Node := TNode.Create;
  Node.Text := ' bunny!';
  Node.Highlight := False;
  FSplits.Add(Node);
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

