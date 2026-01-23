// 标准 MoonBit 测试用例集合
// 为 azimuth 库提供全面的功能测试

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(5, -5))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
}

test "greet_function_different_names" {
  // 测试问候函数的不同名字输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutativity_check" {
  // 测试数学交换律
  let x = 12
  let y = 8
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：计算订单总箱数
  let product_a_qty = 15
  let product_b_qty = 8
  let box_capacity = 10
  
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let total_boxes = add(boxes_a, boxes_b)
  
  assert_eq(3, total_boxes)  // 2 + 1 = 3
}

test "edge_case_calculations" {
  // 测试边界情况计算
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "nested_calculation_scenario" {
  // 测试嵌套计算场景
  let base = 5
  let multiplier = 3
  let addition = 7
  let divisor = 4
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, addition)
  let final_result = divide_with_ceil(step2, divisor)
  
  assert_eq(6, final_result)  // ceil((5*3+7)/4) = ceil(22/4) = 6
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(10, add(10, 0))
  assert_eq(10, add(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 10))
}