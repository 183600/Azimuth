// Enhanced test cases for set module

///|
test "set basic operations" {
  let empty_set : @set.Set[Int] = @set.Set::new()
  let mut int_set = @set.Set::new()
  
  // Test empty set
  inspect(empty_set.length(), content="0")
  inspect(empty_set.is_empty(), content="true")
  
  // Test adding elements
  int_set.insert(1)
  int_set.insert(2)
  int_set.insert(3)
  inspect(int_set.length(), content="3")
  inspect(int_set.is_empty(), content="false")
  
  // Test containing elements
  inspect(int_set.contains(1), content="true")
  inspect(int_set.contains(2), content="true")
  inspect(int_set.contains(3), content="true")
  inspect(int_set.contains(4), content="false")
}

///|
test "set removal operations" {
  let mut int_set = @set.Set::new()
  int_set.insert(1)
  int_set.insert(2)
  int_set.insert(3)
  int_set.insert(4)
  int_set.insert(5)
  
  // Test removing existing elements
  let removed1 = int_set.remove(1)
  let removed2 = int_set.remove(3)
  inspect(removed1, content="true")
  inspect(removed2, content="true")
  inspect(int_set.length(), content="3")
  inspect(int_set.contains(1), content="false")
  inspect(int_set.contains(3), content="false")
  
  // Test removing non-existing element
  let removed3 = int_set.remove(10)
  inspect(removed3, content="false")
  inspect(int_set.length(), content="3")
}

///|
test "set union operations" {
  let mut set1 = @set.Set::new()
  let mut set2 = @set.Set::new()
  
  set1.insert(1)
  set1.insert(2)
  set1.insert(3)
  
  set2.insert(3)
  set2.insert(4)
  set2.insert(5)
  
  // Test union operation
  let union_set = set1.union(set2)
  inspect(union_set.length(), content="5")
  inspect(union_set.contains(1), content="true")
  inspect(union_set.contains(2), content="true")
  inspect(union_set.contains(3), content="true")
  inspect(union_set.contains(4), content="true")
  inspect(union_set.contains(5), content="true")
}