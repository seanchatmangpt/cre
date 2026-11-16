build:
    make compile

test:
    make test

clean:
    make clean

polish:
    make f5_validate

ci: build test
