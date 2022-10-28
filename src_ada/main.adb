with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Assertions;          use Ada.Assertions;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Numerics.Discrete_Random;

with Arenas;

procedure Main is

   -- Entity Environment

   type Alignment is (Invalid, Good, Bad);
   type Alignment_Range is
     range Alignment'Pos (Alignment'Succ (Invalid)) .. Alignment'Pos (Alignment'Last);

   package Alignment_Random is new Ada.Numerics.Discrete_Random (Alignment_Range);
   use Alignment_Random;

   Alignment_Gen : Generator;
   function Random_Alignment return Alignment is (Alignment'Val (Random (Alignment_Gen)));

   type Entity_Environment is record
      Align : Alignment;
   end record;
   Default_Entity_Environment : constant Entity_Environment := (Align => Bad);

   -- Entity

   type Entity is tagged record
      Align : Alignment := Invalid;
      ID    : Integer   := 0;
   end record;

   procedure Entity_Constructor (Self : in out Entity; E : in Entity_Environment) is
   begin
      Self.Align := E.Align;
      if E.Align = Bad then
         Self.ID := 666;
      else
         Self.ID := 1;
      end if;
   end Entity_Constructor;

   procedure Entity_Destructor (Self : in out Entity) is
   begin
      Self.Align := Invalid;
      Self.ID    := -1;
   end Entity_Destructor;

   -- Arenas

   package Entity_Arenas is new Arenas
     (Object            => Entity, Object_Environment => Entity_Environment,
      Max               => Positive'Value (Argument (1)), Object_Constructor => Entity_Constructor,
      Object_Destructor => Entity_Destructor);

   EA  : Entity_Arenas.Arena;
   ERs : Entity_Arenas.Object_References;

begin
   Put_Line (Image (Clock) & ": Running for " & Entity_Arenas.Max_Objects'Image);

   -- Allocate

   for ER of ERs loop
      EA.Allocate (ER, (Align => Random_Alignment));
   end loop;

   Assert (EA.Allocations = EA.Size, EA.Allocations'Image & " /=" & EA.Size'Image);

   -- Use
   -- Toy example to show both Accessors

   for ER of ERs loop
      declare
         EAcc       : Entity_Arenas.Object_Accessor          := ER.Get;
         EAcc_Const : Entity_Arenas.Object_Constant_Accessor := ER.Get_Constant;
      begin
         -- EAcc.Object_Access := null;  -- Mustnt and Doesnt compile
         -- EAcc_Const.Object_Access := null; -- Mustnt Doesnt compile
         -- EAcc_Const.ID := 123;  -- Mustnt and Doesnt compile, though error message isnt very good
         Put_Line (EAcc.Object_Access.all'Image);  -- Q: Any Ada idioms for 'Image?
         EAcc.ID := 123;
         Put_Line (EAcc_Const.Object_Access.all'Image);
         Put_Line ("=========");
      end;
   end loop;

   Assert (for all ER of ERs => ER.Get.Align /= Invalid and ER.Get.ID /= -1);

   -- Deallocate

   for ER of ERs loop
      EA.Deallocate (ER);
      begin
          ER.Get.ID := 123;
      exception
          when E : ASSERTION_ERROR => null; -- Correctly raised
      end;
   end loop;

   Assert (EA.Allocations = 0, EA.Allocations'Image & " /= 0");
   Assert (for all ER of ERs => not EA.Reference_Is_Valid(ER));

end Main;
