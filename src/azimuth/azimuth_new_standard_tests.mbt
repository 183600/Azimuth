// Azimuth 项目标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_test" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "basic_multiplication_test" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "division_ceil_test" {
  // 向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_test" {
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtraction_test" {
  // 减法测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "business_calculation_test" {
  // 业务计算测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "financial_calculation_test" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  
  // 计算利息
  let interest = divide_with_ceil(multiply(principal, rate), 100)
  let total_with_interest = add(principal, interest)
  
  assert_eq(500, interest)
  assert_eq(10500, total_with_interest)
}