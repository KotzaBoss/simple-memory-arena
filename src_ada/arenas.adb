pragma Ada_2022;

package body Arenas is

   ----------------------
   -- Object Reference --
   ----------------------

   function Get (Self : in Object_Reference) return Object_Accessor is
     (Object_Access => Self.O_Access);

   function Get_Constant (Self : in Object_Reference) return Object_Constant_Accessor is
     (Object_Access => Self.O_Access);

   function Is_Null (R : in Object_Reference'Class) return Boolean is
	   (R.O_Access = Null_Object_Reference.O_Access
        and R.Offset = Null_Object_Reference.Offset);

   procedure Nullify (R : out Object_Reference'Class) is
   begin
      R.O_Access := Null_Object_Reference.O_Access;
      R.Offset   := Null_Object_Reference.Offset;
   end Nullify;


   -----------
   -- Arena --
   -----------

   procedure Allocate
     (Self : in out Arena; R : out Object_Reference'Class; OE : in Object_Environment)
   is
      Offset : constant Storage_Offset := Self.First_Available_Offset;
   begin
      if Offset /= Invalid_Storage_Offset then
         Self.Allocation_Flags (Offset) := True;
         Self.Allocation_Counter := @ + 1;

         Object_Constructor (Self.Internal_Storage (Offset), OE);

         -- Q: Is this idiomatic when unable to tag-dispatch due to other parameters dispatch?
         -- Q: Can only use Unchecked_Access to avoid errors, is it correct?
         R.O_Access := Self.Internal_Storage (Offset)'Unchecked_Access;
         R.Offset   := Offset;
      else
         Nullify(R);
      end if;
   end Allocate;

   procedure Deallocate (Self : in out Arena; R : in out Object_Reference'Class) is
   begin
      Self.Allocation_Flags (R.Offset) := False;
      Self.Allocation_Counter := @ - 1;

      Nullify(R);
   end Deallocate;

   function Allocations (Self : in Arena) return Storage_Allocations is (Self.Allocation_Counter);

   function Size (Self : in Arena) return Storage_Size is (Self.Internal_Storage'Length);

   function Reference_Is_Valid
     (Self : in out Arena; R : in Object_Reference'Class) return Boolean is
       (not Is_Null(R)
        and then Self.Internal_Storage (R.Offset)'Unchecked_Access = R.O_Access);

   function First_Available_Offset (Self : in Arena) return Storage_Offset is
   begin
      for I in Self.Allocation_Flags'Range loop
         if not Self.Allocation_Flags (I) then
            return I;
         end if;
      end loop;
      return Invalid_Storage_Offset;
   end First_Available_Offset;

end Arenas;
