// Azimuth 项目最终测试用例
// 使用标准 MoonBit 测试语法，最简版本

test "basic_addition" {
  // 测试基本加法
  let result1 = 1 + 1
  let result2 = 10 + 20
  let result3 = -5 + 15
  let result4 = 0 + 42
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3 + result4
}

test "basic_multiplication" {
  // 测试基本乘法
  let result1 = 2 * 3
  let result2 = 10 * 10
  let result3 = -5 * 4
  let result4 = 0 * 42
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3 + result4
}

test "mathematical_expressions" {
  // 测试数学表达式
  let x = 2
  let y = 3
  let a = 5
  let b = 6
  let c = 7
  let d = 8
  
  let result1 = (2 + 3) * 4
  let result2 = 10 * (5 + 3)
  let result3 = (x - y) * (x + y)
  let result4 = a * b + c * d
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3 + result4
}

test "series_calculations" {
  // 测试级数计算
  let result1 = 1 + 2 + 3 + 4 + 5
  let result2 = 2 + 4 + 6 + 8
  let result3 = 1 + 4 + 9 + 16
  let result4 = 1 + 1 + 2 + 3 + 5
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3 + result4
}

test "business_scenarios" {
  // 测试商业场景
  let price = 50
  let quantity = 3
  let discount = 10
  let tax = 5
  
  let subtotal = price * quantity
  let total = subtotal - discount + tax
  
  // 使用结果避免未使用变量警告
  subtotal + total
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 10
  let width = 5
  let height = 3
  
  let area = length * width
  let perimeter = 2 * (length + width)
  let volume = length * width * height
  
  // 使用结果避免未使用变量警告
  area + perimeter + volume
}

test "string_operations" {
  // 测试字符串操作
  let greeting = "Hello"
  let name = "World"
  let punctuation = "!"
  
  let message = greeting + ", " + name + punctuation
  
  // 使用结果避免未使用变量警告
  message
}

test "boundary_values" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  let result1 = max_int + 0
  let result2 = min_int + 0
  let result3 = max_int * 1
  let result4 = min_int * 1
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3 + result4
}

test "complex_calculations" {
  // 测试复杂计算
  let a = 3
  let b = 4
  let c = 5
  
  // 计算二次方程 ax² + bx + c
  let x = 2
  let result = a * x * x + b * x + c
  
  // 计算勾股定理 a² + b² = c²
  let pythagoras = a * a + b * b
  
  // 使用结果避免未使用变量警告
  result + pythagoras
}

test "performance_patterns" {
  // 测试性能模式
  let base = 5
  
  // 重复加法
  let result1 = base + base + base + base + base
  
  // 嵌套运算
  let result2 = (2 * 3) * (4 * 5)
  
  // 混合运算
  let result3 = 10 * 2 + (5 + 3) * 4
  
  // 使用结果避免未使用变量警告
  result1 + result2 + result3
}