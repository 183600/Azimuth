// Azimuth 项目标准测试用例
// 包含 8 个高质量的 MoonBit 测试用例

test "basic_addition_operations" {
  // 测试基本加法运算
  add(2, 3) = 5
  add(0, 0) = 0
  add(-2, -3) = -5
  add(-3, 13) = 10
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  multiply(2, 3) = 6
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-3, -5) = 15
}

test "greeting_function_tests" {
  // 测试问候函数
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("123") = "Hello, 123!"
}

test "ceil_division_functionality" {
  // 测试向上取整除法功能
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 100) = 1
  divide_with_ceil(5, 0) = 0
}

test "negative_number_division" {
  // 测试负数除法
  divide_with_ceil(-10, 3) = -3
  divide_with_ceil(-10, -3) = 4
  divide_with_ceil(1, -3) = -1
}

test "mathematical_properties" {
  // 测试数学性质
  add(7, 13) = add(13, 7)
  multiply(7, 13) = multiply(13, 7)
}

test "packaging_calculation" {
  // 测试包装计算业务逻辑
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  boxes_needed = 5  // ceil(25/6) = 5
}

test "calculation_pipeline" {
  // 测试计算流水线
  let initial = 10
  let step1 = add(initial, multiply(2, 3))  // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)     // ceil(16/5) = 4
  let result = add(step2, 1)                 // 4 + 1 = 5
  result = 5
}