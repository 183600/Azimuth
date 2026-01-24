// 额外的标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let total_items = 35
  let box_capacity = 8
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(40, total_capacity)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, multiply(-2, -5))
  assert_eq(-10, multiply(-2, 5))
  assert_eq(-2, divide_with_ceil(-10, 5))
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(34, resources_per_team)
  
  // 验证总分配量
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
}

test "sequential_operation_chain" {
  // 测试连续操作链
  let initial = 5
  let step1 = add(initial, 3)        // 8
  let step2 = multiply(step1, 2)     // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let step4 = add(step3, 1)          // 5
  
  assert_eq(5, step4)
}