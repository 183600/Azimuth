// Simple test to verify test system works

///|
test "simple arithmetic" {
  inspect(1 + 1, content="2")
  inspect(2 * 3, content="6")
}

///|
test "string operations" {
  let greeting = "Hello"
  inspect(greeting, content="Hello")
  inspect(greeting.length(), content="5")
}