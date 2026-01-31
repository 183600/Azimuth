// 标准 MoonBit 测试用例 - Azimuth 数学库
// 包含10个高质量测试用例，覆盖核心功能

///|
test "basic_arithmetic_addition" {
  // 测试基本加法运算
  inspect(add(2, 3), content="5")
  inspect(add(0, 0), content="0")
  inspect(add(-5, 5), content="0")
  inspect(add(-3, -7), content="-10")
}

///|
test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  inspect(multiply(4, 5), content="20")
  inspect(multiply(0, 100), content="0")
  inspect(multiply(-3, 7), content="-21")
  inspect(multiply(-4, -6), content="24")
}

///|
test "string_greeting_functionality" {
  // 测试字符串问候功能
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
}

///|
test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法基本功能
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 100), content="1")
  inspect(divide_with_ceil(0, 5), content="0")
}

///|
test "divide_with_ceil_zero_handling" {
  // 测试除零处理
  inspect(divide_with_ceil(5, 0), content="0")
  inspect(divide_with_ceil(-5, 0), content="0")
  inspect(divide_with_ceil(0, 0), content="0")
}

///|
test "subtract_function_basic_operations" {
  // 测试减法函数基本操作
  inspect(subtract(10, 3), content="7")
  inspect(subtract(5, 5), content="0")
  inspect(subtract(3, 10), content="-7")
  inspect(subtract(-5, -3), content="-2")
}

///|
test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  inspect(add(a, b), content="20")
  inspect(add(b, a), content="20")
  inspect(multiply(a, b), content="91")
  inspect(multiply(b, a), content="91")
}

///|
test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let step1 = add(5, 3)           // 8
  let step2 = multiply(step1, 2)  // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let result = add(step3, 1)      // 5
  inspect(result, content="5")
}

///|
test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  inspect(boxes_needed, content="5")
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  inspect(total_capacity, content="30")
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="5")
}

///|
test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  inspect(add(1, 0), content="1")
  inspect(multiply(1, 0), content="0")
  inspect(divide_with_ceil(1, 1), content="1")
  inspect(subtract(1, 1), content="0")
  
  // 大数测试
  inspect(add(1000000, 1000000), content="2000000")
  inspect(multiply(1000, 1000), content="1000000")
  inspect(divide_with_ceil(1000000, 999), content="1001")
}