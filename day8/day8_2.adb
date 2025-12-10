with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Day8_2 is
   type Node is record
      X : Positive;
      Y : Positive;
      Z : Positive;
   end record;

   function "=" (R, L : Node) return Boolean is
   begin
      return R.X = L.X and R.Y = L.Y and R.Z = L.Z;
   end "=";

   function Node_Equ(R, L : Node) return Boolean is
   begin
      return R.X = L.X and R.Y = L.Y and R.Z = L.Z;
   end Node_Equ;

   function Node_Hash (N : Node) return Ada.Containers.Hash_Type is 
   begin
      -- omg fuck this shit, my hash keeps overflowing, I'm just going to make this dumb
      return Ada.Containers.Hash_Type(N.X + N.Y + N.Z);
   end Node_Hash;

   type Edge is record
      Left : Node;
      Right : Node;
      Length: Float;
   end record;

   function "<" (L, R : Edge) return Boolean is 
   begin
      return L.Length < R.Length;
   end "<";

   package NodeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Node);
   ProblemSet : NodeVector.Vector;

   package EdgeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Edge);
   package Edge_Sorting is new EdgeVector.Generic_Sorting;
   Edges : EdgeVector.Vector;

   package NodeHashSet is new Ada.Containers.Hashed_Sets (Element_Type => Node, Equivalent_Elements => Node_Equ, Hash => Node_Hash);

   type Circuit is record
      Nodes : NodeHashSet.Set;
   end record;
   function "<" (L, R : Circuit) return Boolean is 
   begin
      return Integer(L.Nodes.Length) < Integer(R.Nodes.Length);
   end "<";
   package CircuitVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Circuit);
   package CircuitSorting is new CircuitVector.Generic_Sorting;
   Circuits : CircuitVector.Vector;

   function Distance(L, R: Node) return Float is 
   begin
      return Sqrt(Float(Float(L.X - R.X) ** 2 + Float(L.Y - R.Y) ** 2 + Float(L.Z - R.Z) ** 2));
   end Distance;

   Input_File : File_Type;
   File_Name : String := "input.txt";

   function ParseNode (Node_String : String) return Node is 
      To_Return : Node;
      First : Positive := Node_String'First;
      Last : Natural;
   begin
      Last := Ada.Strings.Fixed.Index(Node_String(First..Node_String'Last), ",");
      To_Return.X := Integer'Value(Node_String(First..Last - 1));
      First := Last + 1;

      Last := Ada.Strings.Fixed.Index(Node_String(First..Node_String'Last), "," );
      To_Return.Y := Integer'Value(Node_String(First..Last - 1));
      First := Last + 1;

      To_Return.Z := Integer'Value(Node_String(First..Node_String'Last));

      return To_Return;
   end ParseNode;

   function To_String(N : Node) return String is 
   begin
      return "(" & N.X'Image & "," & N.Y'Image & "," & N.Z'Image & ")"; 
   end To_String;

   function To_String(E : Edge) return String is 
   begin
      return To_String(E.Left) & " -> " & To_String (E.Right) & ": " & E.Length'Image; 
   end To_String;

   function To_String(C : Circuit) return String is 
      Output : Unbounded_String;
   begin
      for I of C.Nodes loop
         Append (Output, To_String(I) & "-");
      end loop;
      return To_String(Output);
   end To_String;

   procedure AddToCircuit(E : Edge) is
      Circ, NewCirc : Circuit;
      Left, Right : Natural;
      HasL, HasR : Boolean := false;
   begin
      for I in Circuits.First_Index..Circuits.Last_Index loop 
         Circ := Circuits(I);
         if Circ.Nodes.Contains (E.Left) then 
            Left := I;
            HasL := true;
         elsif Circ.Nodes.Contains (E.Right) then 
            Right := I;
            HasR := true;
         end if;
      end loop;

      Put_Line (HasL'Image & HasR'Image & To_String (E));

      if HasL and HasR then 
         Circuits(Left).Nodes.Union(Circuits(Right).Nodes);
         Circuits.Delete (Right);

         if Right < Left then 
            Left := Left - 1;
         end if;

         Put_Line (To_String(Circuits(Left)));
      elsif HasL then 
         if not Circuits(Left).Nodes.Contains(E.Right) then 
            Circuits(Left).Nodes.Insert(E.Right);
         end if;
         Put_Line (To_String(Circuits(Left)));
      elsif HasR then
         if not Circuits(Right).Nodes.Contains(E.Left) then 
            Circuits(Right).Nodes.Insert (E.Left);
         end if;
         Put_Line (To_String(Circuits(Right)));
      else 
         NewCirc := (Nodes => NodeHashSet.To_Set(E.Left));
         NewCirc.Nodes.Insert(E.Right);
         Circuits.Append(New_Item => NewCirc);

         Put_Line (To_String(NewCirc));
      end if;
   end;
begin
   Open(Input_File, In_File, File_Name);

   while not End_Of_File(Input_File) loop
      ProblemSet.Append(ParseNode(Get_Line(Input_File)));
   end loop;

   declare 
      Left, Right : Node;
      Dist : Float;
   begin
      for I in ProblemSet.First_Index..ProblemSet.Last_Index loop
         Left := ProblemSet(I);
         for N in I + 1..ProblemSet.Last_Index loop
            Right := ProblemSet(N);
            Dist := Distance (Left, Right);
            Edges.Append (New_Item => (Left => Left, Right => Right, Length => Dist));
            --Put_Line (To_String(Left) & To_String(Right) & Dist'Image);
         end loop;
      end loop;
   end;

   for N of ProblemSet loop
      Circuits.Append(New_Item => (Nodes => NodeHashSet.To_Set(N)));
   end loop;

   Edge_Sorting.Sort (Edges);

   for I in Edges.First_Index..Edges.Last_Index loop
      if I > Edges.Last_Index then 
         exit;
      end if;

      AddToCircuit (Edges(I));

      if Integer(Circuits.Length) = 1 then 
         Put_Line (Long_Integer'Image(Long_Integer((Edges(I).Left.X)) * Long_Integer(Edges(I).Right.X)));
         exit;
      end if;
   end loop;

   CircuitSorting.Sort (Circuits);

   declare 
      Total : Integer := 1;
   begin
      for I in reverse Circuits.Last_Index - 2..Circuits.Last_Index loop
         Put(Circuits(I).Nodes.Length'Image & " *");
         Total := Total * Integer(Circuits(I).Nodes.Length);
      end loop;

      Put_Line ("=" & Total'Image);
   end;
   
end Day8_2;