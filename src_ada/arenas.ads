pragma Ada_2022;

generic
   type Object is private;
   type Object_Environment is private;
   with procedure Object_Constructor (O : in out Object; OE : in Object_Environment);
   with procedure Object_Destructor (O : in out Object);

   Max : Positive;
package Arenas is

   -------------
   -- Storage --
   -------------

   Max_Objects : constant Positive := Max;  -- Q: Can we make the generic visible?

   subtype Storage_Integer is Integer range -1 .. Max_Objects;

   subtype Storage_Size is Storage_Integer range 1 .. Storage_Integer'Last;

   subtype Storage_Offset is Storage_Integer range -1 .. Storage_Size'Last - 1;
   subtype Storage_Valid_Offset is Storage_Offset range 0 .. Storage_Offset'Last;
   Invalid_Storage_Offset : constant Storage_Offset := -1;

   subtype Storage_Allocations is Natural;

   type Storage is array (Storage_Valid_Offset) of aliased Object;
   type Storage_Allocation_Flags is -- Not a standard bitset unfortunately...
     array (Storage_Valid_Offset) of Boolean;
   -- Accessing the flags with an index works when packed, not the for-of loop though. For that to
   -- work the Boolean must be aliased, and then we lose the packing which is more important than
   -- for-of vs for-in sugar.
   pragma Pack (Storage_Allocation_Flags);

   ------------
   -- Object --
   ------------

   type Object_Access is access all Object;

   ---------------------
   -- Object Accessor --
   ---------------------

   -- https://en.m.wikibooks.org/wiki/Ada_Programming/Types/access#Implicit_dereferencing
   -- tldr: This is the common pattern to bring safe access and limit assignment to the pointers
   -- present in the Object_Reference.
   type Object_Accessor (Object_Access : not null access Object) is limited private with
      Implicit_Dereference => Object_Access;
   type Object_Constant_Accessor (Object_Access : not null access constant Object) is
     limited private with
      Implicit_Dereference => Object_Access;

      ----------------------
      -- Object Reference --
      ----------------------

   type Object_Reference is tagged private;
   function Get (Self : in Object_Reference) return Object_Accessor;
   function Get_Constant (Self : in Object_Reference) return Object_Constant_Accessor;
   Null_Object_Reference : constant Object_Reference;

   type Object_References is array (Storage_Valid_Offset) of Object_Reference;

   -----------
   -- Arena --
   -----------

   type Arena is tagged limited private;
   procedure Allocate
     (Self : in out Arena; R : out Object_Reference'Class; OE : in Object_Environment);
   procedure Deallocate (Self : in out Arena; R : in Object_Reference'Class);
   function Allocated (Self : in Arena) return Storage_Allocations;
   function Size (Self : in Arena) return Storage_Size;

private

   ---------------------
   -- Object Accessor --
   ---------------------

   type Object_Accessor (Object_Access : not null access Object) is limited null record;
   type Object_Constant_Accessor (Object_Access : not null access constant Object)
   is limited null record;

   ----------------------
   -- Object Reference --
   ----------------------

   type Object_Reference is tagged record
      O_Access : Object_Access  := null;
      Offset   : Storage_Offset := Invalid_Storage_Offset;
   end record;
   -- Q: Perhaps avoid this repetition?
   Null_Object_Reference : constant Object_Reference :=
     (O_Access => null, Offset => Invalid_Storage_Offset);

   -----------
   -- Arena --
   -----------

   type Arena is tagged limited record
      Internal_Storage : Storage;
      Allocation_Flags : Storage_Allocation_Flags := (others => False);
   end record;

   function Reference_Is_Valid (Self : in out Arena; R : in Object_Reference'Class) return Boolean;
   function Find_Reference_Offset
     (Self : in out Arena; R : in Object_Reference'Class) return Storage_Offset;
   function Find_Available_Offset (Self : in Arena) return Storage_Offset;

end Arenas;
