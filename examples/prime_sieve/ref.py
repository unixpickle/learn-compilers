def sieve(n: int) -> int:
    primes = [True] * n

    # 0 and 1 are not primes
    if n > 0:
        primes[0] = False
    if n > 1:
        primes[1] = False

    p = 2
    while p * p < n:
        if primes[p]:
            j = p * p
            while j < n:
                primes[j] = False
                j += p
        p += 1

    count = 0
    i = 2
    while i < n:
        if primes[i]:
            count += 1
        i += 1

    return count


def main() -> int:
    limit = 50_000_000
    print("counting primes up to", limit)
    c = sieve(limit)
    print("answer:", c)
    return 0


if __name__ == "__main__":
    main()
