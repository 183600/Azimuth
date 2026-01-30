// 标准 MoonBit 测试用例 - 新增10个高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  add(2, 3) = 5
  add(0, 0) = 0
  add(-2, -3) = -5
  add(-5, 15) = 10
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  multiply(2, 3) = 6
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-2, -3) = 6
}

test "greet_function_character_support" {
  // 测试问候函数的字符支持
  greet("World") = "Hello, World!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("") = "Hello, !"
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(5, 0) = 0
  divide_with_ceil(-10, 3) = -3
}

test "subtract_function_verification" {
  // 测试减法函数验证
  subtract(5, 3) = 2
  subtract(3, 5) = -2
  subtract(5, 5) = 0
  subtract(5, -5) = 10
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  add(7, 13) = add(13, 7)
  multiply(7, 13) = multiply(13, 7)
}

test "zero_element_properties" {
  // 测试零元素性质
  add(5, 0) = 5
  multiply(5, 0) = 0
  subtract(5, 0) = 5
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  add(multiply(2, 3), multiply(4, 5)) = 26  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  divide_with_ceil(total_items, items_per_box) = 5  // ceil(25/6) = 5
}

test "business_logic_workflow" {
  // 测试业务逻辑工作流
  let products = 47
  let capacity = 12
  let containers = divide_with_ceil(products, capacity)
  let total_capacity = multiply(containers, capacity)
  let remaining = subtract(total_capacity, products)
  
  containers = 4      // ceil(47/12) = 4
  total_capacity = 48 // 4 * 12 = 48
  remaining = 1       // 48 - 47 = 1
}