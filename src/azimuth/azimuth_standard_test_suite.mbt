// Azimuth 标准 MoonBit 测试套件
// 包含 10 个高质量测试用例，覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  inspect(add(5, 3), content="8")
  inspect(multiply(4, 6), content="24")
  inspect(subtract(10, 7), content="3")
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
}

test "negative_number_operations" {
  // 负数运算测试
  inspect(add(-5, 3), content="-2")
  inspect(add(5, -3), content="2")
  inspect(multiply(-4, 6), content="-24")
  inspect(multiply(4, -6), content="-24")
  inspect(multiply(-4, -6), content="24")
  inspect(subtract(-10, 7), content="-17")
  inspect(subtract(10, -7), content="17")
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  inspect(divide_with_ceil(0, 5), content="0")
  inspect(divide_with_ceil(0, 1), content="0")
  inspect(divide_with_ceil(0, 0), content="0")
  inspect(divide_with_ceil(10, 0), content="0")
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(10, -3), content="3")
  inspect(divide_with_ceil(-10, -3), content="4")
}

test "string_greeting_function" {
  // 字符串问候函数测试
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("中文"), content="Hello, 中文!")
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 15)        // 315
  let step3 = subtract(step2, 5)    // 310
  let step4 = divide_with_ceil(step3, 4)  // 78
  
  inspect(step1, content="300")
  inspect(step2, content="315")
  inspect(step3, content="310")
  inspect(step4, content="78")
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  inspect(add(x, y), content="40")
  inspect(add(y, x), content="40")
  
  // 乘法交换律
  inspect(multiply(x, y), content="391")
  inspect(multiply(y, x), content="391")
  
  // 加法单位元
  inspect(add(x, 0), content="17")
  
  // 乘法单位元
  inspect(multiply(x, 1), content="17")
  
  // 减法的逆运算
  inspect(add(subtract(x, y), y), content="17")
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  inspect(add(0, 0), content="0")
  inspect(multiply(0, 5), content="0")
  inspect(subtract(0, 0), content="0")
  
  // 单位值测试
  inspect(add(1, 0), content="1")
  inspect(multiply(1, 5), content="5")
  inspect(subtract(1, 0), content="1")
  inspect(divide_with_ceil(5, 1), content="5")
  
  // 负一测试
  inspect(add(-1, 1), content="0")
  inspect(multiply(-1, 5), content="-5")
  inspect(subtract(0, -1), content="1")
}

test "large_number_operations" {
  // 大数运算测试
  inspect(add(1000000, 1000000), content="2000000")
  inspect(multiply(10000, 10000), content="100000000")
  inspect(subtract(1000000, 500000), content="500000")
  inspect(divide_with_ceil(1000000, 3), content="333334")
}

test "precision_operations" {
  // 精度运算测试
  inspect(divide_with_ceil(1, 2), content="1")
  inspect(divide_with_ceil(2, 3), content="1")
  inspect(divide_with_ceil(3, 4), content="1")
  inspect(divide_with_ceil(100, 99), content="2")
  inspect(divide_with_ceil(99, 100), content="1")
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 30
  
  // 计算总价
  let total = add(subtotal1, subtotal2)  // 105
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)  // 11
  
  // 计算最终价格
  let final_total = add(total, tax)  // 116
  
  inspect(subtotal1, content="75")
  inspect(subtotal2, content="30")
  inspect(total, content="105")
  inspect(tax, content="11")
  inspect(final_total, content="116")
}