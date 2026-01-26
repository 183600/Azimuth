// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  assert_eq(0, multiply(-10, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test "negative_number_division_comprehensive" {
  // 测试负数除法的全面情况
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.333... -> 4
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0返回0
}

test "large_scale_calculation_test" {
  // 测试大规模计算
  let base = 10000
  let multiplier = 100
  let addition = 50000
  
  // 测试大数乘法
  let large_product = multiply(base, multiplier)
  assert_eq(1000000, large_product)
  
  // 测试大数加法
  let large_sum = add(large_product, addition)
  assert_eq(1050000, large_sum)
  
  // 测试大数除法
  let large_division = divide_with_ceil(large_sum, 10000)
  assert_eq(105, large_division)
}

test "real_world_budget_calculation" {
  // 测试实际世界的预算计算
  let monthly_income = 5000
  let rent = 1500
  let utilities = 200
  let food = 600
  let transportation = 300
  let entertainment = 200
  
  // 计算总支出
  let total_expenses = add(add(add(add(rent, utilities), food), transportation), entertainment)
  
  // 计算剩余预算
  let remaining_budget = add(monthly_income, -total_expenses)
  
  assert_eq(2800, total_expenses)
  assert_eq(2200, remaining_budget)
}

test "mathematical_distributive_property" {
  // 测试数学分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq_string("Hello, !", greet(""))
}