// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "add_with_large_numbers" {
  // 测试大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  assert_eq(2147483647, multiply(46340, 46341))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(-2147483648, multiply(-2, 1073741824))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_unicode_characters" {
  // 测试 Unicode 字符问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列计算
  let a = 5
  let d = 3
  let n = 10
  
  // 计算等差数列第n项: a + (n-1)*d
  let nth_term = add(a, multiply(n - 1, d))
  
  // 计算等差数列前n项和: n/2 * (2a + (n-1)*d)
  let sum = divide_with_ceil(multiply(n, add(multiply(2, a), multiply(n - 1, d))), 2)
  
  assert_eq(32, nth_term)  // 5 + 9*3 = 32
  assert_eq(185, sum)      // 10/2 * (10 + 27) = 5 * 37 = 185
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 600
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(8500, allocated)
  assert_eq(1500, remaining)
  assert_eq(3, additional_projects)
}

test "time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = -8  // 时差
  
  let converted_time = add(local_time, time_difference)
  let adjusted_time = if converted_time < 0 {
    add(converted_time, 24)
  } else if converted_time >= 24 {
    add(converted_time, -24)
  } else {
    converted_time
  }
  
  assert_eq(6, adjusted_time)  // 14 - 8 = 6 (早上6点)
}