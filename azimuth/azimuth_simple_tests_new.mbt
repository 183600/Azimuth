// Azimuth 简单测试用例 - 8个高质量测试用例
// 使用标准的 MoonBit 测试语法和内置断言

test "basic_addition_operations" {
  // 测试基本的加法运算
  @inspect(add(3, 5))
  @inspect(add(0, 0))
  @inspect(add(-3, -5))
  @inspect(add(-3, 5))
  @inspect(add(500, 500))
}

test "basic_multiplication_operations" {
  // 测试基本的乘法运算
  @inspect(multiply(3, 5))
  @inspect(multiply(3, 0))
  @inspect(multiply(-3, 5))
  @inspect(multiply(-3, -5))
  @inspect(multiply(100, 100))
}

test "division_with_ceil_operations" {
  // 测试向上取整除法运算
  @inspect(divide_with_ceil(10, 4))
  @inspect(divide_with_ceil(12, 4))
  @inspect(divide_with_ceil(0, 5))
  @inspect(divide_with_ceil(10, 0))
  @inspect(divide_with_ceil(-7, 4))
}

test "greet_function_tests" {
  // 测试问候函数
  @inspect(greet("MoonBit"))
  @inspect(greet(""))
  @inspect(greet("123"))
  @inspect(greet("世界"))
  @inspect(greet("🚀"))
}

test "mathematical_commutative_property" {
  // 测试数学交换律
  let a = 7
  let b = 11
  
  // 加法交换律
  @inspect(add(a, b))
  @inspect(add(b, a))
  
  // 乘法交换律
  @inspect(multiply(a, b))
  @inspect(multiply(b, a))
}

test "packaging_calculation" {
  // 包装计算业务场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  @inspect(boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  @inspect(total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  @inspect(remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 10
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  @inspect(step1)
  @inspect(step2)
  @inspect(result)
}

test "event_planning_scenario" {
  // 活动规划场景
  let participants = 85
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  @inspect(tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  @inspect(available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  @inspect(remaining_seats)
}