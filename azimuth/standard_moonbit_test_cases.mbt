// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含基本功能测试和边界情况测试

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
}

test "mathematical_commutative_property" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(7, add(7, 0))
  assert_eq(0, multiply(7, 0))
  assert_eq(7, subtract(7, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let initial = 10
  let doubled = multiply(initial, 2)
  let added = add(doubled, 5)
  let divided = divide_with_ceil(added, 3)
  assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(1, divide_with_ceil(-5, -3))
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let items = 23
  let capacity_per_box = 5
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}