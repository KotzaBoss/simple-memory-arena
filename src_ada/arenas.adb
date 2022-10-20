pragma Ada_2022;

with Ada.Assertions; use Ada.Assertions;

package body Arenas is

   ----------------------
   -- Object Reference --
   ----------------------

   function Get (Self : in Object_Reference) return Object_Accessor is
     (Object_Access => Self.O_Access);

   function Get_Constant (Self : in Object_Reference) return Object_Constant_Accessor is
     (Object_Access => Self.O_Access);

   -----------
   -- Arena --
   -----------

   procedure Allocate
     (Self : in out Arena; R : out Object_Reference'Class; OE : in Object_Environment)
   is
      Offset : constant Storage_Offset := Self.Find_Available_Offset;
   begin
      if Offset /= Invalid_Storage_Offset then
         Self.Allocation_Flags (Offset) := True;
         Object_Constructor (Self.Internal_Storage (Offset), OE);
         -- Q: Is this idiomatic when unable to tag-dispatch due to other parameters dispatch?
         -- Q: Can only use Unchecked_Access to avoid errors, is it correct?
         R.O_Access := Self.Internal_Storage (Offset)'Unchecked_Access;
         R.Offset   := Offset;
      else
         R.O_Access := Null_Object_Reference.O_Access;
         R.Offset   := Null_Object_Reference.Offset;
      end if;
   end Allocate;

   procedure Deallocate (Self : in out Arena; R : in Object_Reference'Class) is
      Offset : Storage_Offset := Self.Find_Reference_Offset (R);
   begin
      Assert (Offset /= Invalid_Storage_Offset);

      Self.Allocation_Flags (Offset) := False;
      Object_Destructor (Self.Internal_Storage (Offset));
   end Deallocate;

   function Allocated (Self : in Arena) return Storage_Allocations is
      C : Storage_Allocations := 0;
   begin
      -- Q: Any Ada idioms/tricks for simplicity?
      for I in Self.Allocation_Flags'Range loop
         if Self.Allocation_Flags (I) then
            C := @ + 1;
         end if;
      end loop;
      return C;
   end Allocated;

   function Size (Self : in Arena) return Storage_Size is (Self.Internal_Storage'Length);

   function Reference_Is_Valid
     (Self : in out Arena; R : in Object_Reference'Class) return Boolean is
     (for some I in Self.Internal_Storage'Range =>
        I = R.Offset and Self.Internal_Storage (I)'Unchecked_Access = R.O_Access);

   function Find_Reference_Offset
     (Self : in out Arena; R : in Object_Reference'Class) return Storage_Offset
   is
   begin
      for I in Self.Internal_Storage'Range loop
         if Self.Internal_Storage (I)'Unchecked_Access = R.O_Access then
            return I;
         end if;
      end loop;
      return Invalid_Storage_Offset;
   end Find_Reference_Offset;

   function Find_Available_Offset (Self : in Arena) return Storage_Offset is
   begin
      for I in Self.Allocation_Flags'Range loop
         if not Self.Allocation_Flags (I) then
            return I;
         end if;
      end loop;
      return Invalid_Storage_Offset;
   end Find_Available_Offset;

end Arenas;
