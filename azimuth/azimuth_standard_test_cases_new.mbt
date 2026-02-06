// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法
// 覆盖基本功能和一些边缘情况

test "azimuth_basic_add_commutative" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
}

test "azimuth_multiply_distributive" {
  // 测试乘法分配律
  let a = 5
  let b = 8
  let c = 3
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "azimuth_divide_ceil_zero_cases" {
  // 测试除零和零除法的边缘情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "azimuth_divide_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(34, divide_with_ceil(99, 3))
  assert_eq(34, divide_with_ceil(98, 3))
  assert_eq(33, divide_with_ceil(97, 3))
}

test "azimuth_greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "azimuth_subtract_negative_results" {
  // 测试减法结果的负数情况
  assert_eq(-5, subtract(10, 15))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(25, subtract(10, -15))
}

test "azimuth_complex_calculation_sequence" {
  // 测试复杂计算序列
  let initial = 12
  let step1 = multiply(initial, 3)  // 36
  let step2 = add(step1, 8)         // 44
  let step3 = divide_with_ceil(step2, 10)  // 5
  let step4 = subtract(step3, 2)    // 3
  assert_eq(3, step4)
}

test "azimuth_resource_allocation_scenario" {
  // 测试资源分配场景
  let total_tasks = 87
  let team_capacity = 20
  let weeks_needed = divide_with_ceil(total_tasks, team_capacity)
  
  assert_eq(5, weeks_needed)
  
  // 验证总容量
  let total_capacity = multiply(weeks_needed, team_capacity)
  assert_eq(100, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(13, remaining_capacity)
}

test "azimuth_mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "azimuth_large_number_operations" {
  // 测试大数运算
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000, multiply(big_num, big_num))
  assert_eq(100001, divide_with_ceil(add(big_num, 1), 1))
  assert_eq(0, subtract(big_num, big_num))
}