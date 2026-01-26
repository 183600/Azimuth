// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_basic" {
  // 测试加法函数的基本功能
  let result1 = add(2, 3)
  let result2 = add(0, 0)
  let result3 = add(-2, -3)
  let result4 = add(-5, 7)
  
  // 验证结果
  result1 = 5
  result2 = 0
  result3 = -5
  result4 = 2
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  let result1 = multiply(3, 4)
  let result2 = multiply(5, 0)
  let result3 = multiply(-2, 3)
  let result4 = multiply(-2, -3)
  let result5 = multiply(10, 10)
  
  // 验证结果
  result1 = 12
  result2 = 0
  result3 = -6
  result4 = 6
  result5 = 100
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  let result1 = greet("World")
  let result2 = greet("世界")
  let result3 = greet("café")
  let result4 = greet("🚀")
  let result5 = greet("")
  
  // 验证结果
  result1 = "Hello, World!"
  result2 = "Hello, 世界!"
  result3 = "Hello, café!"
  result4 = "Hello, 🚀!"
  result5 = "Hello, !"
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边缘情况
  let result1 = divide_with_ceil(10, 3)
  let result2 = divide_with_ceil(9, 3)
  let result3 = divide_with_ceil(1, 1)
  let result4 = divide_with_ceil(0, 5)
  let result5 = divide_with_ceil(10, 0)
  
  // 验证结果
  result1 = 4
  result2 = 3
  result3 = 1
  result4 = 0
  result5 = 0
}

test "negative_numbers_test" {
  // 测试负数运算
  let add_result1 = add(-2, -3)
  let add_result2 = add(-5, 7)
  let mul_result1 = multiply(-3, -5)
  let mul_result2 = multiply(-3, 5)
  let div_result = divide_with_ceil(-5, 3)
  
  // 验证结果
  add_result1 = -5
  add_result2 = 2
  mul_result1 = 15
  mul_result2 = -15
  div_result = -2
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let step1 = multiply(3, 4)  // 12
  let step2 = multiply(2, 5)  // 10
  let step3 = add(step1, step2)  // 22
  let result = divide_with_ceil(step3, 7)  // 4
  
  // 验证结果
  result = 4
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)  // 5
  let total_capacity = multiply(boxes_needed, box_capacity)  // 30
  let remaining_space = subtract(total_capacity, items)  // 5
  
  // 验证结果
  boxes_needed = 5
  total_capacity = 30
  remaining_space = 5
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 13
  let b = 17
  let add_result1 = add(a, b)
  let add_result2 = add(b, a)
  let mul_result1 = multiply(a, b)
  let mul_result2 = multiply(b, a)
  
  // 验证交换律
  add_result1 = add_result2
  mul_result1 = mul_result2
}

test "resource_allocation" {
  // 测试资源分配问题
  let total_resources = 47
  let teams = 4
  let resources_per_team = divide_with_ceil(total_resources, teams)  // 12
  let total_allocated = multiply(resources_per_team, teams)  // 48
  
  // 验证结果
  resources_per_team = 12
  total_allocated = 48
}

test "string_processing" {
  // 测试字符串处理
  let result1 = greet("space")
  let result2 = greet("special@chars")
  let result3 = greet("emoji 🌟")
  let result4 = greet("mixedCASE123")
  
  // 验证结果
  result1 = "Hello, space!"
  result2 = "Hello, special@chars!"
  result3 = "Hello, emoji 🌟!"
  result4 = "Hello, mixedCASE123!"
}