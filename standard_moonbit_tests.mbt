// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  inspect(add(2, 3), content="5")
  inspect(add(0, 0), content="0")
  inspect(add(-2, -3), content="-5")
  inspect(add(10, -5), content="5")
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  inspect(multiply(2, 3), content="6")
  inspect(multiply(0, 5), content="0")
  inspect(multiply(-2, 3), content="-6")
  inspect(multiply(-2, -3), content="6")
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  inspect(greet("World"), content="Hello, World!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 100), content="1")
  inspect(divide_with_ceil(25, 6), content="5")
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(-10, -3), content="4")
  inspect(divide_with_ceil(5, 0), content="0")
  inspect(divide_with_ceil(-9, 3), content="-3")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  inspect(add(a, b), content="20")
  inspect(add(b, a), content="20")
  
  // 乘法交换律
  inspect(multiply(a, b), content="91")
  inspect(multiply(b, a), content="91")
  
  // 零元素性质
  inspect(multiply(a, 0), content="0")
  inspect(add(a, 0), content="7")
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  inspect(result1, content="26")  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  inspect(result2, content="16")  // (5 + 3) * 2 = 8 * 2 = 16
}

test "practical_business_scenario" {
  // 实际业务场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  inspect(subtotal, content="200")
  inspect(total, content="215")
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  inspect(boxes_needed, content="5")  // ceil(25/6) = 5
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  inspect(greet("世界"), content="Hello, 世界!")
  inspect(greet("🚀"), content="Hello, 🚀!")
  inspect(greet("123"), content="Hello, 123!")
  inspect(greet("test@example.com"), content="Hello, test@example.com!")
}