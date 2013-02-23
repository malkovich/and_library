(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDBinTre                                                         *)
(* Binary tree classes, including splay tree and red-black tree     *)
(********************************************************************)

unit TDBinTre;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  TDBasics;

type
  TtdChildType = (      {types of children}
         ctLeft,        {..left child}
         ctRight);      {..right child}

  TtdRBColor = (        {colors for the red-black tree}
         rbBlack,       {..black}
         rbRed);        {..red}

  TtdTraversalMode = (  {different traversal modes..}
         tmPreOrder,    {..pre-order}
         tmInOrder,     {..in-order}
         tmPostOrder,   {..post-order}
         tmLevelOrder); {..level-order}

  PtdBinTreeNode = ^TtdBinTreeNode; {binary tree node}
  TtdBinTreeNode = packed record
    btParent : PtdBinTreeNode;
    btChild  : array [TtdChildType] of PtdBinTreeNode;
    btData   : pointer;
    case boolean of
      false : (btExtra  : longint);
      true  : (btColor  : TtdRBColor);
  end;

type
  TtdBinaryTree = class
    {the binary tree class}
    private
      FCount   : integer;
      FDispose : TtdDisposeProc;
      FHead    : PtdBinTreeNode;
      FName    : TtdNameString;
    protected
      procedure btError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      function btLevelOrder(aAction    : TtdVisitProc;
                            aExtraData : pointer) : PtdBinTreeNode;
      function btNoRecInOrder(aAction    : TtdVisitProc;
                              aExtraData : pointer) : PtdBinTreeNode;
      function btNoRecPostOrder(aAction    : TtdVisitProc;
                                aExtraData : pointer) : PtdBinTreeNode;
      function btNoRecPreOrder(aAction    : TtdVisitProc;
                               aExtraData : pointer) : PtdBinTreeNode;
      function btRecInOrder(aNode      : PtdBinTreeNode;
                            aAction    : TtdVisitProc;
                            aExtraData : pointer) : PtdBinTreeNode;
      function btRecPostOrder(aNode      : PtdBinTreeNode;
                              aAction    : TtdVisitProc;
                              aExtraData : pointer) : PtdBinTreeNode;
      function btRecPreOrder(aNode      : PtdBinTreeNode;
                             aAction    : TtdVisitProc;
                             aExtraData : pointer) : PtdBinTreeNode;
    public
      constructor Create(aDisposeItem : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      procedure Delete(aNode : PtdBinTreeNode);
      function InsertAt(aParentNode : PtdBinTreeNode;
                        aChildType  : TtdChildType;
                        aItem       : pointer) : PtdBinTreeNode;
      function Root : PtdBinTreeNode;
      function Traverse(aMode         : TtdTraversalMode;
                        aAction       : TtdVisitProc;
                        aExtraData    : pointer;
                        aUseRecursion : boolean) : PtdBinTreeNode;

      property Count : integer
                  read FCount;
      property Name : TtdNameString
                  read FName write FName;
  end;

  TtdBinarySearchTree = class
    {binary search tree class}
    private
      FBinTree : TtdBinaryTree;
      FCompare : TtdCompareFunc;
      FCount   : integer;
      FName    : TtdNameString;
    protected
      procedure bstError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      function bstFindItem(aItem  : pointer;
                       var aNode  : PtdBinTreeNode;
                       var aChild : TtdChildType) : boolean;
      function bstFindNodeToDelete(aItem : pointer) : PtdBinTreeNode;
      function bstInsertPrim(aItem      : pointer;
                         var aChildType : TtdChildType)
                                        : PtdBinTreeNode;
    public
      constructor Create(aCompare : TtdCompareFunc;
                         aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      procedure Delete(aItem : pointer); virtual;
      function Find(aKeyItem : pointer) : pointer; virtual;
      procedure Insert(aItem : pointer); virtual;
      function Traverse(aMode         : TtdTraversalMode;
                        aAction       : TtdVisitProc;
                        aExtraData    : pointer;
                        aUseRecursion : boolean) : pointer;

      property BinaryTree : TtdBinaryTree
                  read FBinTree;
      property Count : integer
                  read FCount;
      property Name : TtdNameString
                  read FName write FName;
  end;

  TtdSplayTree = class(TtdBinarySearchTree)
    private
    protected
      function stPromote(aNode  : PtdBinTreeNode) : PtdBinTreeNode;
      procedure stSplay(aNode : PtdBinTreeNode);
    public
      procedure Delete(aItem : pointer); override;
      function Find(aKeyItem : pointer) : pointer; override;
      procedure Insert(aItem : pointer); override;
  end;

  TtdRedBlackTree = class(TtdBinarySearchTree)
    {the red-black tree class--a balanced binary search tree}
    private
    protected
      function rbtPromote(aNode  : PtdBinTreeNode) : PtdBinTreeNode;
    public
      procedure Delete(aItem : pointer); override;
      procedure Insert(aItem : pointer); override;
  end;


type
  TtdDrawBinaryNode = procedure (aNode  : PtdBinTreeNode;
                                 aStrip : integer;
                                 aColumn: integer;
                                 aParentStrip : integer;
                                 aParentColumn: integer;
                                 aExtraData   : pointer);

procedure DrawBinaryTree(aTree      : TObject;
                         aDrawNode  : TtdDrawBinaryNode;
                         aExtraData : pointer);


implementation

uses
  TDNdeMgr,
  TDStkQue;

const
  UnitName = 'TDBinTre';

var
  BTNodeManager : TtdNodeManager; {nodemanager for binary trees}

{===Helper routines==================================================}
function GetChildType(aNode : PtdBinTreeNode) : TtdChildType;
begin
  if (aNode^.btParent^.btChild[ctLeft] = aNode) then
    Result := ctLeft
  else
    Result := ctRight;
end;
{--------}
function IsRed(aNode : PtdBinTreeNode) : boolean;
begin
  if (aNode = nil) then
    Result := false
  else
    Result := aNode^.btColor = rbRed;
end;
{====================================================================}


{===TtdBinaryTree====================================================}
constructor TtdBinaryTree.Create(aDisposeItem : TtdDisposeProc);
begin
  inherited Create;
  FDispose := aDisposeItem;
  {make sure the node manager is available}
  if (BTNodeManager = nil) then
    BTNodeManager := TtdNodeManager.Create(sizeof(TtdBinTreeNode));
  {allocate a head node, eventually the root node of the tree will be
   its left child}
  FHead := BTNodeManager.AllocNodeClear;
end;
{--------}
destructor TtdBinaryTree.Destroy;
begin
  Clear;
  BTNodeManager.FreeNode(FHead);
  inherited Destroy;
end;
{--------}
procedure TtdBinaryTree.btError(aErrorCode  : integer;
                          const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdBinTreeException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdBinaryTree.btLevelOrder(aAction    : TtdVisitProc;
                                    aExtraData : pointer)
                                               : PtdBinTreeNode;
var
  Queue   : TtdQueue;
  Node    : PtdBinTreeNode;
  StopNow : boolean;
begin
  {ASSUMPTION: FCount <> 0}
  {assume we won't get a node selected}
  Result := nil;
  {create the queue}
  StopNow := false;
  Queue := TtdQueue.Create(nil);
  Queue.Name := ClassName + ': levelorder queue';
  try
    {enqueue the root}
    Queue.Enqueue(FHead^.btChild[ctLeft]);
    {continue until the queue is empty}
    while not Queue.IsEmpty do begin
      {get the node at the head of the queue}
      Node := Queue.Dequeue;
      {perform the action on it, if this returns with a request to
       stop then return this node}
      aAction(Node^.btData, aExtraData, StopNow);
      if StopNow then begin
        Result := Node;
        Queue.Clear;
      end
      {otherwise, continue}
      else begin
        {enqueue the left child, if it's not nil}
        if (Node^.btChild[ctLeft] <> nil) then
          Queue.Enqueue(Node^.btChild[ctLeft]);
        {enqueue the right child, if it's not nil}
        if (Node^.btChild[ctRight] <> nil) then
          Queue.Enqueue(Node^.btChild[ctRight]);
      end;
    end;
  finally
    {destroy the queue}
    Queue.Free;
  end;
end;
{--------}
function TtdBinaryTree.btNoRecInOrder(aAction    : TtdVisitProc;
                                      aExtraData : pointer)
                                                 : PtdBinTreeNode;
var
  Stack   : TtdStack;
  Node    : PtdBinTreeNode;
  StopNow : boolean;
begin
  {ASSUMPTION: FCount <> 0}
  {assume we won't get a node selected}
  Result := nil;
  {create the stack}
  StopNow := false;
  Stack := TtdStack.Create(nil);
  Stack.Name := ClassName + ': inorder stack';
  try
    {push the root}
    Stack.Push(FHead^.btChild[ctLeft]);
    {continue until the stack is empty}
    while not Stack.IsEmpty do begin
      {get the node at the head of the queue}
      Node := Stack.Pop;
      {if it's nil, pop the next node, perform the action on it, if
       this returns with a request to stop then return this node}
      if (Node = nil) then begin
        Node := Stack.Pop;
        aAction(Node^.btData, aExtraData, StopNow);
        if StopNow then begin
          Result := Node;
          Stack.Clear;
        end;
      end
      {otherwise, the children of the node have not been pushed yet}
      else begin
        {push the right child, if it's not nil}
        if (Node^.btChild[ctRight] <> nil) then
          Stack.Push(Node^.btChild[ctRight]);
        {push the node, followed by a nil pointer}
        Stack.Push(Node);
        Stack.Push(nil);
        {push the left child, if it's not nil}
        if (Node^.btChild[ctLeft] <> nil) then
          Stack.Push(Node^.btChild[ctLeft]);
      end;
    end;
  finally
    {destroy the stack}
    Stack.Free;
  end;
end;
{--------}
function TtdBinaryTree.btNoRecPostOrder(aAction    : TtdVisitProc;
                                        aExtraData : pointer)
                                                   : PtdBinTreeNode;
var
  Stack   : TtdStack;
  Node    : PtdBinTreeNode;
  StopNow : boolean;
begin
  {ASSUMPTION: FCount <> 0}
  {assume we won't get a node selected}
  Result := nil;
  {create the stack}
  StopNow := false;
  Stack := TtdStack.Create(nil);
  Stack.Name := ClassName + ': postorder stack';
  try
    {push the root}
    Stack.Push(FHead^.btChild[ctLeft]);
    {continue until the stack is empty}
    while not Stack.IsEmpty do begin
      {get the node at the head of the queue}
      Node := Stack.Pop;
      {if it's nil, pop the next node, perform the action on it, if
       this returns false (ie, don't continue), return this node}
      if (Node = nil) then begin
        Node := Stack.Pop;
        aAction(Node^.btData, aExtraData, StopNow);
        if StopNow then begin
          Result := Node;
          Stack.Clear;
        end;
      end
      {otherwise, the children of the node have not been pushed yet}
      else begin
        {push the node, followed by a nil pointer}
        Stack.Push(Node);
        Stack.Push(nil);
        {push the right child, if it's not nil}
        if (Node^.btChild[ctRight] <> nil) then
          Stack.Push(Node^.btChild[ctRight]);
        {push the left child, if it's not nil}
        if (Node^.btChild[ctLeft] <> nil) then
          Stack.Push(Node^.btChild[ctLeft]);
      end;
    end;
  finally
    {destroy the stack}
    Stack.Free;
  end;
end;
{--------}
function TtdBinaryTree.btNoRecPreOrder(aAction    : TtdVisitProc;
                                       aExtraData : pointer)
                                                  : PtdBinTreeNode;
var
  Stack   : TtdStack;
  Node    : PtdBinTreeNode;
  StopNow : boolean;
begin
  {ASSUMPTION: FCount <> 0}
  {assume we won't get a node selected}
  Result := nil;
  {create the stack}
  StopNow := false;
  Stack := TtdStack.Create(nil);
  Stack.Name := ClassName + ': preorder stack';
  try
    {push the root}
    Stack.Push(FHead^.btChild[ctLeft]);
    {continue until the stack is empty}
    while not Stack.IsEmpty do begin
      {get the node at the head of the queue}
      Node := Stack.Pop;
      {perform the action on it, if this returns false (ie, don't
       continue), return this node}
      aAction(Node^.btData, aExtraData, StopNow);
      if StopNow then begin
        Result := Node;
        Stack.Clear;
      end
      {otherwise, continue}
      else begin
        {push the right child, if it's not nil}
        if (Node^.btChild[ctRight] <> nil) then
          Stack.Push(Node^.btChild[ctRight]);
        {push the left child, if it's not nil}
        if (Node^.btChild[ctLeft] <> nil) then
          Stack.Push(Node^.btChild[ctLeft]);
      end;
    end;
  finally
    {destroy the stack}
    Stack.Free;
  end;
end;
{--------}
function TtdBinaryTree.btRecInOrder(aNode      : PtdBinTreeNode;
                                    aAction    : TtdVisitProc;
                                    aExtraData : pointer)
                                               : PtdBinTreeNode;
var
  StopNow : boolean;
begin
  Result := nil;
  if (aNode^.btChild[ctLeft] <> nil) then begin
    Result := btRecInOrder(aNode^.btChild[ctLeft],
                           aAction, aExtraData);
    if (Result <> nil) then
      Exit;
  end;
  StopNow := false;
  aAction(aNode^.btData, aExtraData, StopNow);
  if StopNow then begin
    Result := aNode;
    Exit;
  end;
  if (aNode^.btChild[ctRight] <> nil) then begin
    Result := btRecInOrder(aNode^.btChild[ctRight],
                           aAction, aExtraData);
  end;
end;
{--------}
function TtdBinaryTree.btRecPostOrder(aNode      : PtdBinTreeNode;
                                      aAction    : TtdVisitProc;
                                      aExtraData : pointer)
                                                 : PtdBinTreeNode;
var
  StopNow : boolean;
begin
  Result := nil;
  if (aNode^.btChild[ctLeft] <> nil) then begin
    Result := btRecPostOrder(aNode^.btChild[ctLeft],
                             aAction, aExtraData);
    if (Result <> nil) then
      Exit;
  end;
  if (aNode^.btChild[ctRight] <> nil) then begin
    Result := btRecPostOrder(aNode^.btChild[ctRight],
                             aAction, aExtraData);
    if (Result <> nil) then
      Exit;
  end;
  StopNow := false;
  aAction(aNode^.btData, aExtraData, StopNow);
  if StopNow then
    Result := aNode;
end;
{--------}
function TtdBinaryTree.btRecPreOrder(aNode      : PtdBinTreeNode;
                                     aAction    : TtdVisitProc;
                                     aExtraData : pointer)
                                                : PtdBinTreeNode;
var
  StopNow : boolean;
begin
  Result := nil;
  StopNow := false;
  aAction(aNode^.btData, aExtraData, StopNow);
  if StopNow then begin
    Result := aNode;
    Exit;
  end;
  if (aNode^.btChild[ctLeft] <> nil) then begin
    Result := btRecPreOrder(aNode^.btChild[ctLeft],
                            aAction, aExtraData);
    if (Result <> nil) then
      Exit;
  end;
  if (aNode^.btChild[ctRight] <> nil) then begin
    Result := btRecPreOrder(aNode^.btChild[ctRight],
                            aAction, aExtraData);
  end;
end;
{--------}
procedure TtdBinaryTree.Clear;
var
  Stack : TtdStack;
  Node  : PtdBinTreeNode;
begin
  {to clear a binary tree, we perform a postorder traversal where to
   visit a node means to free it; and to be safe we'll make it a
   non-recursive one}
  if (FCount = 0) then
    Exit;
  {create the stack}
  Stack := TtdStack.Create(nil);
  Stack.Name := ClassName + ': clear stack';
  try
    {push the root}
    Stack.Push(FHead^.btChild[ctLeft]);
    {continue until the stack is empty}
    while not Stack.IsEmpty do begin
      {get the node at the head of the queue}
      Node := Stack.Pop;
      {if it's nil, pop the next node and free it}
      if (Node = nil) then begin
        Node := Stack.Pop;
        if Assigned(FDispose) then
          FDispose(Node^.btData);
        BTNodeManager.FreeNode(Node);
      end
      {otherwise, the children of the node have not been pushed yet}
      else begin
        {push the node, followed by a nil pointer}
        Stack.Push(Node);
        Stack.Push(nil);
        {push the right child, if it's not nil}
        if (Node^.btChild[ctRight] <> nil) then
          Stack.Push(Node^.btChild[ctRight]);
        {push the left child, if it's not nil}
        if (Node^.btChild[ctLeft] <> nil) then
          Stack.Push(Node^.btChild[ctLeft]);
      end;
    end;
  finally
    {destroy the stack}
    Stack.Free;
  end;
  {patch up the tree to be empty}
  FCount := 0;
  FHead^.btChild[ctLeft] := nil;
end;
{--------}
procedure TtdBinaryTree.Delete(aNode : PtdBinTreeNode);
var
  OurChildsType : TtdChildType;
  OurType       : TtdChildType;
begin
  if (aNode = nil) then
    Exit;
  {find out whether we have a single child and which one it is; if we
   find that there are two children raise an exception}
  if (aNode^.btChild[ctLeft] <> nil) then begin
    if (aNode^.btChild[ctRight] <> nil) then
      btError(tdeBinTree2Children, 'Delete');
    OurChildsType := ctLeft;
  end
  else
    OurChildsType := ctRight;
  {find out whether we're a left or right child of our parent}
  OurType := GetChildType(aNode);
  {set the child link of our parent to our child link}
  aNode^.btParent^.btChild[OurType] := aNode^.btChild[OurChildsType];
  if (aNode^.btChild[OurChildsType] <> nil) then
    aNode^.btChild[OurChildsType]^.btParent := aNode^.btParent;
  {free the node}
  if Assigned(FDispose) then
    FDispose(aNode^.btData);
  BTNodeManager.FreeNode(aNode);
  dec(FCount);
end;
{--------}
function TtdBinaryTree.InsertAt(aParentNode : PtdBinTreeNode;
                                aChildType  : TtdChildType;
                                aItem       : pointer)
                                            : PtdBinTreeNode;
begin
  {if the parent node is nil, assume this is inserting the root}
  if (aParentNode = nil) then begin
    aParentNode := FHead;
    aChildType := ctLeft;
  end;
  {check to see the child link isn't already set}
  if (aParentNode^.btChild[aChildType] <> nil) then
    btError(tdeBinTreeHasChild, 'InsertAt');
  {allocate a new node and insert as the required child of the parent}
  Result := BTNodeManager.AllocNode;
  Result^.btParent := aParentNode;
  Result^.btChild[ctLeft] := nil;
  Result^.btChild[ctRight] := nil;
  Result^.btData := aItem;
  Result^.btExtra := 0;
  aParentNode^.btChild[aChildType] := Result;
  inc(FCount);
end;
{--------}
function TtdBinaryTree.Root : PtdBinTreeNode;
begin
  Result := FHead^.btChild[ctLeft];
end;
{--------}
function TtdBinaryTree.Traverse(aMode         : TtdTraversalMode;
                                aAction       : TtdVisitProc;
                                aExtraData    : pointer;
                                aUseRecursion : boolean)
                                              : PtdBinTreeNode;
var
  RootNode : PtdBinTreeNode;
begin
  Result := nil;
  RootNode := FHead^.btChild[ctLeft];
  if (RootNode <> nil) then begin
    case aMode of
      tmPreOrder :
        if aUseRecursion then
          Result := btRecPreOrder(RootNode, aAction, aExtraData)
        else
          Result := btNoRecPreOrder(aAction, aExtraData);
      tmInOrder :
        if aUseRecursion then
          Result := btRecInOrder(RootNode, aAction, aExtraData)
        else
          Result := btNoRecInOrder(aAction, aExtraData);
      tmPostOrder :
        if aUseRecursion then
          Result := btRecPostOrder(RootNode, aAction, aExtraData)
        else
          Result := btNoRecPostOrder(aAction, aExtraData);
      tmLevelOrder :
        Result := btLevelOrder(aAction, aExtraData);
    end;
  end;
end;
{====================================================================}


{===TtdBinarySearchTree==============================================}
constructor TtdBinarySearchTree.Create(aCompare : TtdCompareFunc;
                                       aDispose : TtdDisposeProc);
begin
  inherited Create;
  FCompare := aCompare;
  FBinTree := TtdBinaryTree.Create(aDispose);
  FBinTree.Name := ClassName + ': delegate tree';
end;
{--------}
destructor TtdBinarySearchTree.Destroy;
begin
  FBinTree.Free;
  inherited Destroy;
end;
{--------}
procedure TtdBinarySearchTree.bstError(aErrorCode  : integer;
                                 const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdBinTreeException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdBinarySearchTree.bstFindItem(aItem  : pointer;
                                     var aNode  : PtdBinTreeNode;
                                     var aChild : TtdChildType)
                                                : boolean;
var
  Walker    : PtdBinTreeNode;
  CmpResult : integer;
begin
  Result := false;
  {if the tree is empty, return nil and left to signify that a new
   node, if inserted, would be the root}
  if (FCount = 0) then begin
    aNode := nil;
    aChild := ctLeft;
    Exit;
  end;
  {otherwise, wlk the tree}
  Walker := FBinTree.Root;
  CmpResult := FCompare(aItem, Walker^.btData);
  while (CmpResult <> 0) do begin
    if (CmpResult < 0) then begin
      if (Walker^.btChild[ctLeft] = nil) then begin
        aNode := Walker;
        aChild := ctLeft;
        Exit;
      end;
      Walker := Walker^.btChild[ctLeft];
    end
    else begin
      if (Walker^.btChild[ctRight] = nil) then begin
        aNode := Walker;
        aChild := ctRight;
        Exit;
      end;
      Walker := Walker^.btChild[ctRight];
    end;
    CmpResult := FCompare(aItem, Walker^.btData);
  end;
  Result := true;
  aNode := Walker;
end;
{--------}
function TtdBinarySearchTree.bstFindNodeToDelete(aItem : pointer)
                                                     : PtdBinTreeNode;
var
  Walker : PtdBinTreeNode;
  Node   : PtdBinTreeNode;
  Temp   : pointer;
  ChildType : TtdChildType;
begin
  {attempt to find the item; signal error if not found}
  if not bstFindItem(aItem, Node, ChildType) then
    bstError(tdeBinTreeItemMissing, 'bstFindNodeToDelete');
  {if the node has two children, find the largest node that is smaller
   than the one we want to delete, and swap over the items}
  if (Node^.btChild[ctLeft] <> nil) and
     (Node^.btChild[ctRight] <> nil) then begin
    Walker := Node^.btChild[ctLeft];
    while (Walker^.btChild[ctRight] <> nil) do
      Walker := Walker^.btChild[ctRight];
    Temp := Walker^.btData;
    Walker^.btData := Node^.btData;
    Node^.btData := Temp;
    Node := Walker;
  end;
  {return the node to delete}
  Result := Node;
end;
{--------}
function TtdBinarySearchTree.bstInsertPrim(aItem      : pointer;
                                       var aChildType : TtdChildType)
                                                     : PtdBinTreeNode;
begin
  {first, attempt to find the item; if found, it's an error}
  if bstFindItem(aItem, Result, aChildType) then
    bstError(tdeBinTreeDupItem, 'bstInsertPrim');
  {this returns a node, so insert there}
  Result := FBinTree.InsertAt(Result, aChildType, aItem);
  inc(FCount);
end;
{--------}
procedure TtdBinarySearchTree.Clear;
begin
  FBinTree.Clear;
  FCount := 0;
end;
{--------}
procedure TtdBinarySearchTree.Delete(aItem : pointer);
begin
  FBinTree.Delete(bstFindNodeToDelete(aItem));
  dec(FCount);
end;
{--------}
function TtdBinarySearchTree.Find(aKeyItem : pointer) : pointer;
var
  Node      : PtdBinTreeNode;
  ChildType : TtdChildType;
begin
  if bstFindItem(aKeyItem, Node, ChildType) then
    Result := Node^.btData
  else
    Result := nil;
end;
{--------}
procedure TtdBinarySearchTree.Insert(aItem : pointer);
var
  ChildType : TtdChildType;
begin
  bstInsertPrim(aItem, ChildType);
end;
{--------}
function TtdBinarySearchTree.Traverse(aMode        : TtdTraversalMode;
                                      aAction      : TtdVisitProc;
                                      aExtraData   : pointer;
                                      aUseRecursion: boolean)
                                                   : pointer;
var
  Node : PtdBinTreeNode;
begin
  Node :=
     FBinTree.Traverse(aMode, aAction, aExtraData, aUseRecursion);
  if (Node = nil) then
    Result := nil
  else
    Result := Node^.btData;
end;
{====================================================================}

{====================================================================}
procedure TtdSplayTree.Delete(aItem : pointer);
var
  Node : PtdBinTreeNode;
  Dad  : PtdBinTreeNode;
begin
  Node := bstFindNodeToDelete(aItem);
  Dad := Node^.btParent;
  FBinTree.Delete(Node);
  dec(FCount);
  if (Count <> 0) then
    stSplay(Dad);
end;
{--------}
function TtdSplayTree.Find(aKeyItem : pointer) : pointer;
var
  Node      : PtdBinTreeNode;
  ChildType : TtdChildType;
begin
  if bstFindItem(aKeyItem, Node, ChildType) then begin
    Result := Node^.btData;
    stSplay(Node);
  end
  else
    Result := nil;
end;
{--------}
procedure TtdSplayTree.Insert(aItem : pointer);
var
  ChildType : TtdChildType;
begin
  stSplay(bstInsertPrim(aItem, ChildType));
end;
{--------}
function TtdSplayTree.stPromote(aNode  : PtdBinTreeNode)
                                       : PtdBinTreeNode;
var
  Parent : PtdBinTreeNode;
begin
  {make a note of the parent of the node we're promoting}
  Parent := aNode^.btParent;

  {in both cases there are 6 links to be broken and remade: the node's
   link to its child and vice versa, the node's link with its parent
   and vice versa and the parent's link with its parent and vice
   versa; note that the node's child could be nil}

  {promote a left child = right rotation of parent}
  if (Parent^.btChild[ctLeft] = aNode) then begin
    Parent^.btChild[ctLeft] := aNode^.btChild[ctRight];
    if (Parent^.btChild[ctLeft] <> nil) then
      Parent^.btChild[ctLeft]^.btParent := Parent;
    aNode^.btParent := Parent^.btParent;
    if (aNode^.btParent^.btChild[ctLeft] = Parent) then
      aNode^.btParent^.btChild[ctLeft] := aNode
    else
      aNode^.btParent^.btChild[ctRight] := aNode;
    aNode^.btChild[ctRight] := Parent;
    Parent^.btParent := aNode;
  end
  {promote a right child = left rotation of parent}
  else begin
    Parent^.btChild[ctRight] := aNode^.btChild[ctLeft];
    if (Parent^.btChild[ctRight] <> nil) then
      Parent^.btChild[ctRight]^.btParent := Parent;
    aNode^.btParent := Parent^.btParent;
    if (aNode^.btParent^.btChild[ctLeft] = Parent) then
      aNode^.btParent^.btChild[ctLeft] := aNode
    else
      aNode^.btParent^.btChild[ctRight] := aNode;
    aNode^.btChild[ctLeft] := Parent;
    Parent^.btParent := aNode;
  end;
  {return the node we promoted}
  Result := aNode;
end;
{--------}
procedure TtdSplayTree.stSplay(aNode : PtdBinTreeNode);
var
  Dad      : PtdBinTreeNode;
  Grandad  : PtdBinTreeNode;
  RootNode : PtdBinTreeNode;
begin
  {as we've got to play until we reach the root, get the root as a
   local variable: it'll make things a little faster}
  RootNode := FBinTree.Root;
  {if we're at the root, there's no splaying to do}
  if (aNode = RootNode) then
    Exit;
  {get the parent and the grandparent}
  Dad := aNode^.btParent;
  if (Dad = RootNode) then
    Grandad := nil
  else
    Grandad := Dad^.btParent;
  {while we can, perform zig-zag and zig-zig promotions}
  while (Grandad <> nil) do begin
    {determine the kind of double-promotion we need to do}
    if ((Grandad^.btChild[ctLeft] = Dad) and
        (Dad^.btChild[ctLeft] = aNode)) or
       ((Grandad^.btChild[ctRight] = Dad) and
        (Dad^.btChild[ctRight] = aNode)) then begin
      {zig-zig promotion}
      stPromote(Dad);
      stPromote(aNode);
    end
    else begin
      {zig-zag promotion}
      stPromote(stPromote(aNode));
    end;
    {now we've promoted the node, get the new parent and grandparent}
    RootNode := FBinTree.Root;
    if (aNode = RootNode) then begin
      Dad := nil;
      Grandad := nil;
    end
    else begin
      Dad := aNode^.btParent;
      if (Dad = RootNode) then
        Grandad := nil
      else
        Grandad := Dad^.btParent;
    end;
  end;
  {once this point is reached, the node is either at the root, or one
   level down; make one last promotion if necessary}
  if (Dad <> nil) then
    stPromote(aNode);
end;
{====================================================================}


{===TtdRedBlackTree==================================================}
procedure TtdRedBlackTree.Delete(aItem : pointer);
var
  Node       : PtdBinTreeNode;
  Dad        : PtdBinTreeNode;
  Child      : PtdBinTreeNode;
  Brother    : PtdBinTreeNode;
  FarNephew  : PtdBinTreeNode;
  NearNephew : PtdBinTreeNode;
  IsBalanced : boolean;
  ChildType  : TtdChildType;
begin
  {find the node to delete; this node will have but one child}
  Node := bstFindNodeToDelete(aItem);
  {if the node is red, or is the root, delete it with impunity}
  if (Node^.btColor = rbRed) or (Node = FBinTree.Root) then begin
    FBinTree.Delete(Node);
    dec(FCount);
    Exit;
  end;
  {if the node's only child is red, recolor the child black, and
   delete the node}
  if (Node^.btChild[ctLeft] = nil) then
    Child := Node^.btChild[ctRight]
  else
    Child := Node^.btChild[ctLeft];
  if IsRed(Child) then begin
    Child^.btColor := rbBlack;
    FBinTree.Delete(Node);
    dec(FCount);
    Exit;
  end;

  {at this point, the node we have to delete is Node, it is black, and
   we know that its one Child that will replace it is black (and also
   maybe nil!), and there is a parent of Node (which will soon be the
   parent of Child); Node's brother also exists because of the black
   node rule}

  {if the Child is nil, we'll have to help the loop a little bit and
   set the parent and brother and whether Node is a left child or not}
  if (Child = nil) then begin
    Dad := Node^.btParent;
    if (Node = Dad^.btChild[ctLeft]) then begin
      ChildType := ctLeft;
      Brother := Dad^.btChild[ctRight];
    end
    else begin
      ChildType := ctRight;
      Brother := Dad^.btChild[ctLeft];
    end;
  end
  else begin
    {the following three lines are merely to fool the compiler and
     remove some spurious warnings}
    Dad := nil;
    Brother := nil;
    ChildType := ctLeft;
  end;
  {delete the node we want to remove, we have no more need of it}
  FBinTree.Delete(Node);
  dec(FCount);
  Node := Child;
  {in a loop, continue applying the red-black deletion balancing
   algorithms until the tree is balanced}
  repeat
    {assume we'll balance it this time}
    IsBalanced := true;
    {we are balanced if the node is the root, so assume it isn't}
    if (Node <> FBinTree.Root) then begin
      {get the parent and the brother}
      if (Node <> nil) then begin
        Dad := Node^.btParent;
        if (Node = Dad^.btChild[ctLeft]) then begin
          ChildType := ctLeft;
          Brother := Dad^.btChild[ctRight];
        end
        else begin
          ChildType := ctRight;
          Brother := Dad^.btChild[ctLeft];
        end;
      end;
      {we need a black brother, so if the brother is currently red,
       color the parent red, the brother black, and promote the
       brother; then go round loop again}
      if (Brother^.btColor = rbRed) then begin
        Dad^.btColor := rbRed;
        Brother^.btColor := rbBlack;
        rbtPromote(Brother);
        IsBalanced := false;
      end
      {otherwise the brother is black}
      else begin
        {get the nephews, denoted as far and near}
        if (ChildType = ctLeft) then begin
          FarNephew := Brother^.btChild[ctRight];
          NearNephew := Brother^.btChild[ctLeft];
        end
        else begin
          FarNephew := Brother^.btChild[ctLeft];
          NearNephew := Brother^.btChild[ctRight];
        end;
        {if the far nephew is red (note that it could be nil!), color
         it black, color the brother the same as the parent, color the
         parent black, and then promote the brother; we're then done}
        if IsRed(FarNephew) then begin
          FarNephew^.btColor := rbBlack;
          Brother^.btColor := Dad^.btColor;
          Dad^.btColor := rbBlack;
          rbtPromote(Brother);
        end
        {otherwise the far nephew is black}
        else begin
          {if the near nephew is red (note that it could be nil!),
           color it the same color as the parent, color the parent
           black, and zig-zag promote the nephew; we're then done}
          if IsRed(NearNephew) then begin
            NearNephew^.btColor := Dad^.btColor;
            Dad^.btColor := rbBlack;
            rbtPromote(rbtPromote(NearNephew));
          end
          {otherwise the near nephew is also black}
          else begin
            {if the parent is red, color it black and the brother red,
             and we're done}
            if (Dad^.btColor = rbRed) then begin
              Dad^.btColor := rbBlack;
              Brother^.btColor := rbRed;
            end
            {otherwise the parent is black: color the brother red and
             start over with the parent}
            else begin
              Brother^.btColor := rbRed;
              Node := Dad;
              IsBalanced := false;
            end;
          end;
        end;
      end;
    end;
  until IsBalanced;
end;
{--------}
procedure TtdRedBlackTree.Insert(aItem : pointer);
var
  Node     : PtdBinTreeNode;
  Dad      : PtdBinTreeNode;
  Grandad  : PtdBinTreeNode;
  Uncle    : PtdBinTreeNode;
  OurType  : TtdChildType;
  DadsType : TtdChildType;
  IsBalanced : boolean;
begin
  {insert the new item, get back the node that was inserted and its
   relationship to its parent}
  Node := bstInsertPrim(aItem, OurType);

  {color it red}
  Node^.btColor := rbRed;

  {in a loop, continue applying the red-black insertion balancing
   algorithms until the tree is balanced}
  repeat
    {assume we'll balance it this time}
    IsBalanced := true;
    {if the node is the root, we're done and the tree is balanced, so
     assume we're not at the root}
    if (Node <> FBinTree.Root) then begin
      {as we're not at the root, get the parent of this node}
      Dad := Node^.btParent;
      {if the parent is black, we're done and the tree is balanced, so
       assume that the parent is red}
      if (Dad^.btColor = rbRed) then begin
        {if the parent is the root, just recolor it black and we're
         done}
        if (Dad = FBinTree.Root) then
          Dad^.btColor := rbBlack
        {otherwise the parent has a parent of its own}
        else begin
          {get the grandparent (it must be black) and color it red}
          Grandad := Dad^.btParent;
          Grandad^.btColor := rbRed;
          {get the uncle node}
          if (Grandad^.btChild[ctLeft] = Dad) then begin
            DadsType := ctLeft;
            Uncle := Grandad^.btChild[ctRight];
          end
          else begin
            DadsType := ctRight;
            Uncle := Grandad^.btChild[ctLeft];
          end;
          {if the uncle is also red (note that the uncle can be nil!),
           color the parent black, the uncle black and start over with
           the grandparent}
          if IsRed(Uncle) then begin
            Dad^.btColor := rbBlack;
            Uncle^.btColor := rbBlack;
            Node := Grandad;
            IsBalanced := false;
          end
          {otherwise the uncle is black}
          else begin
            {if the current node has the same relationship with its
             parent as the parent has with the grandparent (ie, both
             are left children or both are right children), color the
             parent black and promote it; we're then done}
            OurType := GetChildType(Node);
            if (OurType = DadsType) then begin
              Dad^.btColor := rbBlack;
              rbtPromote(Dad);
            end
            {otherwise color the node black and zig-zag promote it;
             we're then done}
            else begin
              Node^.btColor := rbBlack;
              rbtPromote(rbtPromote(Node));
            end;
          end;
        end;
      end;
    end;
  until IsBalanced;
end;
{--------}
function TtdRedBlackTree.rbtPromote(aNode : PtdBinTreeNode)
                                          : PtdBinTreeNode;
var
  Parent : PtdBinTreeNode;
begin
  {make a note of the parent of the node we're promoting}
  Parent := aNode^.btParent;

  {in both cases there are 6 links to be broken and remade: the node's
   link to its child and vice versa, the node's link with its parent
   and vice versa and the parent's link with its parent and vice
   versa; note that the node's child could be nil}

  {promote a left child = right rotation of parent}
  if (Parent^.btChild[ctLeft] = aNode) then begin
    Parent^.btChild[ctLeft] := aNode^.btChild[ctRight];
    if (Parent^.btChild[ctLeft] <> nil) then
      Parent^.btChild[ctLeft]^.btParent := Parent;
    aNode^.btParent := Parent^.btParent;
    if (aNode^.btParent^.btChild[ctLeft] = Parent) then
      aNode^.btParent^.btChild[ctLeft] := aNode
    else
      aNode^.btParent^.btChild[ctRight] := aNode;
    aNode^.btChild[ctRight] := Parent;
    Parent^.btParent := aNode;
  end
  {promote a right child = left rotation of parent}
  else begin
    Parent^.btChild[ctRight] := aNode^.btChild[ctLeft];
    if (Parent^.btChild[ctRight] <> nil) then
      Parent^.btChild[ctRight]^.btParent := Parent;
    aNode^.btParent := Parent^.btParent;
    if (aNode^.btParent^.btChild[ctLeft] = Parent) then
      aNode^.btParent^.btChild[ctLeft] := aNode
    else
      aNode^.btParent^.btChild[ctRight] := aNode;
    aNode^.btChild[ctLeft] := Parent;
    Parent^.btParent := aNode;
  end;
  {return the node we promoted}
  Result := aNode;
end;
{====================================================================}


{===Drawing a binary tree============================================}
type
  PNodePosn = ^TNodePosn;
  TNodePosn = packed record
    npStrip  : integer;
    npColumn : integer;
  end;
{--------}
procedure DrawBinaryTree(aTree      : TObject;
                         aDrawNode  : TtdDrawBinaryNode;
                         aExtraData : pointer);
  {------}
  function GenPosNode(aNode   : PtdBinTreeNode;
                      aStrip  : integer;
                  var aColumn : integer) : PtdBinTreeNode;
  var
    OurPosNode : PtdBinTreeNode;
    OurPosition : PNodePosn;
  begin
    {allocate ourselves a node and a position}
    OurPosNode := BTNodeManager.AllocNode;
    FillChar(OurPosNode^, sizeof(OurPosNode^), 0);
    New(OurPosition);
    OurPosNode^.btData := OurPosition;

    {visit the left subtree}
    if (aNode^.btChild[ctLeft] <> nil) then begin
      OurPosNode^.btChild[ctLeft] :=
         GenPosNode(aNode^.btChild[ctLeft], succ(aStrip), aColumn);
      OurPosNode^.btChild[ctLeft]^.btParent := OurPosNode;
    end;

    {store our position, increment the column since we're there now}
    OurPosition^.npStrip := aStrip;
    OurPosition^.npColumn := aColumn;
    inc(aColumn);

    {visit the right subtree}
    if (aNode^.btChild[ctRight] <> nil) then begin
      OurPosNode^.btChild[ctRight] :=
        GenPosNode(aNode^.btChild[ctRight], succ(aStrip), aColumn);
      OurPosNode^.btChild[ctRight]^.btParent := OurPosNode;
    end;

    Result := OurPosNode;
  end;
  {------}
  procedure DestroyPosNode(aNode : PtdBinTreeNode);
  begin
    {destroy the left subtree}
    if (aNode^.btChild[ctLeft] <> nil) then
      DestroyPosNode(aNode^.btChild[ctLeft]);
    {destroy the right subtree}
    if (aNode^.btChild[ctRight] <> nil) then
      DestroyPosNode(aNode^.btChild[ctRight]);
    {destroy this node}
    Dispose(PNodePosn(aNode^.btData));
    BTNodeManager.FreeNode(aNode);
  end;
  {------}
var
  BinTree : TtdBinaryTree;
  Strip, Column : integer;
  PStrip, PColumn : integer;
  PosRoot : PtdBinTreeNode;
  Queue   : TtdQueue;
  Node    : PtdBinTreeNode;
  PosNode : PtdBinTreeNode;
begin
  {get a hold of the actual binary tree}
  if (aTree is TtdBinaryTree) then
    BinTree := TtdBinaryTree(aTree)
  else if (aTree is TtdBinarySearchTree) then
    BinTree := TtdBinarySearchTree(aTree).BinaryTree
  else
    Exit;

  {simple case first}
  if (BinTree.Count = 0) then
    Exit;

  {--first pass--}
  Strip := 0;
  Column := 0;
  PosRoot := GenPosNode(BinTree.Root, Strip, Column);

  {--second pass--}
  try
    {create the queue}
    Queue := TtdQueue.Create(nil);
    try
      {enqueue the roots}
      Queue.Enqueue(BinTree.Root);
      Queue.Enqueue(PosRoot);
      {continue until the queue is empty}
      while not Queue.IsEmpty do begin
        {get the nodes at the head of the queue}
        Node := Queue.Dequeue;
        PosNode := Queue.Dequeue;
        {draw the node}
        if (PosNode = PosRoot) then begin
          PStrip := -1;
          PColumn := -1;
        end
        else with PNodePosn(PosNode^.btParent^.btData)^ do begin
          PStrip := npStrip;
          PColumn := npColumn;
        end;
        with PNodePosn(PosNode^.btData)^ do
          aDrawNode(Node, npStrip, npColumn,
                          PStrip, PColumn, aExtraData);
        {enqueue the left children, if the first is not nil}
        if (Node^.btChild[ctLeft] <> nil) then begin
          Queue.Enqueue(Node^.btChild[ctLeft]);
          Queue.Enqueue(PosNode^.btChild[ctLeft]);
        end;
        {enqueue the right children, if the first is not nil}
        if (Node^.btChild[ctRight] <> nil) then begin
          Queue.Enqueue(Node^.btChild[ctRight]);
          Queue.Enqueue(PosNode^.btChild[ctRight]);
        end;
      end;
    finally
      {destroy the queue}
      Queue.Free;
    end;
  finally
    {now destroy the position binary tree}
    DestroyPosNode(PosRoot);
  end;
end;
{====================================================================}


procedure FinalizeUnit; far;
begin
  BTNodeManager.Free;
end;

initialization
  BTNodeManager := nil;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.

