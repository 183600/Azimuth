// Enhanced test cases for tuple module

///|
test "tuple pattern matching" {
  let point2d = (3, 4)
  let point3d = (1, 2, 3)
  let mixed = ("hello", 42, true)
  
  // Pattern match on 2-tuple
  let sum2 = match point2d {
    (x, y) => x + y
  }
  inspect(sum2, content="7")
  
  // Pattern match on 3-tuple
  let sum3 = match point3d {
    (x, y, z) => x + y + z
  }
  inspect(sum3, content="6")
  
  // Pattern match on mixed types
  let result = match mixed {
    (s, n, b) => if b { s.length() + n } else { n }
  }
  inspect(result, content="47")
}

///|
test "tuple nested operations" {
  let nested = ((1, 2), (3, 4))
  let deep = (((1, 2), (3, 4)), ((5, 6), (7, 8)))
  
  // Access nested tuple elements
  let first_inner = nested.0
  let second_inner = nested.1
  inspect(first_inner, content="(1, 2)")
  inspect(second_inner, content="(3, 4)")
  
  // Access deeply nested elements
  let deep_first = deep.0.0
  let deep_last = deep.1.1
  inspect(deep_first, content="(1, 2)")
  inspect(deep_last, content="(7, 8)")
  
  // Modify nested tuples
  let modified = ((first_inner.0 * 2, first_inner.1 * 2), second_inner)
  inspect(modified, content="((2, 4), (3, 4))")
}

///|
test "tuple with complex types" {
  let array_tuple = ([1, 2, 3], ["a", "b", "c"])
  let option_tuple = (Some(42), None)
  let result_tuple = (Ok("success"), Err("failure"))
  
  // Work with array tuple
  let sum_first = array_tuple.0.fold((acc, x) => acc + x, init=0)
  let concat_second = array_tuple.1.fold((acc, s) => acc + s, init="")
  inspect(sum_first, content="6")
  inspect(concat_second, content="abc")
  
  // Work with option tuple
  let first_value = option_tuple.0.unwrap_or(0)
  let second_value = option_tuple.1.unwrap_or(99)
  inspect(first_value, content="42")
  inspect(second_value, content="99")
  
  // Work with result tuple
  let ok_value = match result_tuple.0 {
    Ok(v) => v
    Err(_) => "error"
  }
  let err_value = match result_tuple.1 {
    Ok(v) => v
    Err(e) => e
  }
  inspect(ok_value, content="success")
  inspect(err_value, content="failure")
}