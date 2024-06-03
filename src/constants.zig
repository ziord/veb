pub const ListVar = @as([]const u8, "List");
pub const TupleVar = @as([]const u8, "Tuple");
pub const ErrorVar = @as([]const u8, "Error");
pub const MapVar = @as([]const u8, "Map");
pub const JustVar = @as([]const u8, "Just");
pub const NoneVar = @as([]const u8, "None");
pub const MaybeVar = @as([]const u8, "Maybe");
pub const OkVar = @as([]const u8, "Ok");
pub const ResultVar = @as([]const u8, "Result");
pub const KeyVar = @as([]const u8, "Key");
pub const ValueVar = @as([]const u8, "Value");
pub const StrVar = @as([]const u8, "str");
pub const NumVar = @as([]const u8, "num");
pub const BoolVar = @as([]const u8, "bool");
pub const AnyVar = @as([]const u8, "any");
pub const NilVar = @as([]const u8, "nil");
pub const SelfVar = @as([]const u8, "self");
pub const InitVar = @as([]const u8, "init");
pub const NeverVar = @as([]const u8, "never");
pub const TrueVar = @as([]const u8, "true");
pub const FalseVar = @as([]const u8, "false");
pub const NoReturnVar = @as([]const u8, "noreturn");
pub const VoidVar = @as([]const u8, "void");
pub const PanicVar = @as([]const u8, "@panic");
pub const GeneratedTypeVar = @as([]const u8, "<>");
pub const UnderscoreVar = @as([]const u8, "_");
pub const LenVar = @as([]const u8, "len");
pub const LambdaVar = @as([]const u8, "<anon>");
pub const ScriptVar = @as([]const u8, "<script>");
pub const GeneratedVarMarker = '$';

pub const MAX_STR_LEN = 0x1000000;
pub const MAX_IDENT_LEN = 0x100;