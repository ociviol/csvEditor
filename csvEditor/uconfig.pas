unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$if defined(Linux) or defined(Darwin)}
  cthreads,
  {$endif}
  Utils.Json;

type

  { TConfig }

  TConfig = Class(TJsonObject)
  private
    FBottom,
    FLeft,
    FRight,
    FTop: Integer;
    FUseFormulas : Boolean;
    FRecent : TStringlist;
  public
    constructor Create;
    destructor Destroy; override;

    function GetBoundsRect:Trect;
    procedure SetBoundsRect(const Value : Trect);
    procedure AddRecent(const aFilename : String);
  published
    property Left : Integer read FLeft write FLeft;
    property Top : Integer read FTop write FTop;
    property Right : Integer read FRight write FRight;
    property Bottom : Integer read FBottom write FBottom;
    property Recent : TStringlist read FRecent write FRecent;
    property UseFormulas : Boolean read FUseFormulas write FUseFormulas;
  end;

implementation

{ TConfig }

constructor TConfig.Create;
begin
  inherited Create;
  FRecent := TStringlist.Create;
  FUseFormulas := false;
end;

destructor TConfig.Destroy;
begin
  FRecent.Free;
  inherited Destroy;
end;

function TConfig.GetBoundsRect: Trect;
begin
  result := Rect(FLeft, FTop, FRight, FBottom);
end;

procedure TConfig.SetBoundsRect(const Value: Trect);
begin
  Fleft := Value.Left;
  FTop := Value.Top;
  FRight := Value.Right;
  FBottom := Value.Bottom;
end;

procedure TConfig.AddRecent(const aFilename: String);
begin
  with FRecent do
    if IndexOf(aFilename) < 0 then
    begin
      if Count > 10 then
        Delete(0);
      Add(aFilename);
    end;
end;


end.

