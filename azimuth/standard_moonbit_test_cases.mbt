// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试加法函数的基本功能
  let result1 = add(2, 3)
  let result2 = add(0, 0)
  let result3 = add(-5, 10)
  let result4 = add(1000, 2000)
  
  // 验证结果
  inspect(result1, expected = 5)
  inspect(result2, expected = 0)
  inspect(result3, expected = 5)
  inspect(result4, expected = 3000)
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  let result1 = multiply(4, 5)
  let result2 = multiply(0, 100)
  let result3 = multiply(-3, 7)
  let result4 = multiply(-4, -6)
  
  // 验证结果
  inspect(result1, expected = 20)
  inspect(result2, expected = 0)
  inspect(result3, expected = -21)
  inspect(result4, expected = 24)
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  let result1 = greet("World")
  let result2 = greet("世界")
  let result3 = greet("🚀")
  let result4 = greet("")
  
  // 验证结果
  inspect(result1, expected = "Hello, World!")
  inspect(result2, expected = "Hello, 世界!")
  inspect(result3, expected = "Hello, 🚀!")
  inspect(result4, expected = "Hello, !")
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  let result1 = divide_with_ceil(10, 3)
  let result2 = divide_with_ceil(9, 3)
  let result3 = divide_with_ceil(1, 100)
  let result4 = divide_with_ceil(0, 5)
  
  // 验证结果
  inspect(result1, expected = 4)
  inspect(result2, expected = 3)
  inspect(result3, expected = 1)
  inspect(result4, expected = 0)
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  let result1 = divide_with_ceil(-10, 3)
  let result2 = divide_with_ceil(10, -3)
  let result3 = divide_with_ceil(-10, -3)
  let result4 = divide_with_ceil(5, 0)
  
  // 验证结果
  inspect(result1, expected = -3)
  inspect(result2, expected = -4)
  inspect(result3, expected = 4)
  inspect(result4, expected = 0)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  let commutative_add = add(x, y) == add(y, x)
  
  // 乘法交换律
  let commutative_mul = multiply(x, y) == multiply(y, x)
  
  // 分配律
  let distributive = multiply(x, add(y, 2)) == add(multiply(x, y), multiply(x, 2))
  
  // 验证性质
  inspect(commutative_add, expected = true)
  inspect(commutative_mul, expected = true)
  inspect(distributive, expected = true)
}

test "business_packaging_scenario" {
  // 业务场景：包装计算
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_items)
  
  // 验证计算结果
  inspect(boxes_needed, expected = 4)
  inspect(total_capacity, expected = 48)
  inspect(remaining_space, expected = 1)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  // 验证结果：ceil(10*3/4) + 5 = ceil(30/4) + 5 = 8 + 5 = 13
  inspect(result, expected = 13)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  let total_allocated = multiply(max_share, team_members)
  let budget_overflow = total_allocated > total_budget
  
  // 验证分配结果
  inspect(max_share, expected = 1667)
  inspect(total_allocated, expected = 10002)
  inspect(budget_overflow, expected = true)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零情况
  let zero_div_1 = divide_with_ceil(10, 0)
  let zero_div_2 = divide_with_ceil(-10, 0)
  let zero_div_3 = divide_with_ceil(0, 0)
  
  // 极值测试
  let extreme_1 = divide_with_ceil(1, 1)
  let extreme_2 = divide_with_ceil(-1, -1)
  let extreme_3 = divide_with_ceil(0, 1)
  
  // 验证错误处理
  inspect(zero_div_1, expected = 0)
  inspect(zero_div_2, expected = 0)
  inspect(zero_div_3, expected = 0)
  inspect(extreme_1, expected = 1)
  inspect(extreme_2, expected = 1)
  inspect(extreme_3, expected = 0)
}