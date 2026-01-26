// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(0, -456))
  assert_eq(0, multiply(-456, 0))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(50, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let x = 12
  let y = 8
  let z = 5
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "large_number_calculations" {
  // 测试大数计算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(200001, 2))
}

test "mathematical_distributive_property" {
  // 测试分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "real_world_shopping_cart" {
  // 实际购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 4
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(75, subtotal1)
  assert_eq(60, subtotal2)
  assert_eq(135, total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, boxes_needed)
}