fn sum(numbers []int) int {
    total = 0
    for number in numbers {
        total += number
    }
    return total
}

fn main() {
    numbers = [1, 2, 3, 4, 5]
    total = sum(numbers)
    print("Total is {total}")
}