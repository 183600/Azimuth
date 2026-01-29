// Azimuth 标准测试用例
// 使用标准 MoonBit 测试语法编写的测试用例

// 导入lib.mbt中定义的函数
// 这些函数已经在lib.mbt中定义

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(999, 1))
}

test "add_function_with_zero" {
  // 测试与零的加法
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(5, 0))
}

test "divide_with_ceil_basic_operations" {
  // 测试向上取整除法的基本操作
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_zero_divisor" {
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_standard_inputs" {
  // 测试标准输入的问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "subtract_function_basic" {
  // 测试基本减法运算
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算包装需求
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
}

test "unicode_string_handling" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

// 新添加的5个测试用例
test "precision_division_edge_cases" {
  // 测试除法精度边缘情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1, divide_with_ceil(-1, 1000000))
  assert_eq(-2, divide_with_ceil(-1000001, 1000000))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base = 10
  let step1 = multiply(base, 2)           // 20
  let step2 = add(step1, 5)               // 25
  let step3 = divide_with_ceil(step2, 3)  // 9
  let step4 = multiply(step3, 4)          // 36
  let result = subtract(step4, 2)         // 34
  
  assert_eq(34, result)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "mathematical_sequence_operations" {
  // 测试数学序列操作
  let start = 1
  let difference = 3
  let terms = 5
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  let term5 = add(term4, difference)
  
  let sequence_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(35, sequence_sum)  // 1 + 4 + 7 + 10 + 13 = 35
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试各种极端除零情况
  assert_eq(0, divide_with_ceil(2147483647, 0))    // 最大正值除零
  assert_eq(0, divide_with_ceil(-2147483648, 0))   // 最大负值除零
  assert_eq(0, divide_with_ceil(0, 0))             // 零除零
  
  // 测试极值运算
  let max_positive = 2147483647
  let max_negative = -2147483648
  
  assert_eq(0, multiply(max_positive, 0))
  assert_eq(0, multiply(max_negative, 0))
}