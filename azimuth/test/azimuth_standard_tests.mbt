test "azimuth_add_basic" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "azimuth_multiply_basic" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "azimuth_greet_basic" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "azimuth_divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "azimuth_subtract_basic" {
  @azimuth.assert_eq(-1, @azimuth.subtract(2, 3))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(15, 5))
}

test "azimuth_commutative_property" {
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "azimuth_packaging_scenario" {
  let items = 25
  let capacity = 6
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, boxes)
  
  let total_capacity = @azimuth.multiply(boxes, capacity)
  @azimuth.assert_eq(30, total_capacity)
  
  let remaining = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(5, remaining)
}

test "azimuth_complex_workflow" {
  let base = 5
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(6, result)
}

test "azimuth_edge_cases" {
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "azimuth_boundary_values" {
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}