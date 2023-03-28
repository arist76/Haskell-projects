DROP TABLE IF EXISTS book;
DROP TABLE IF EXISTS member;
DROP TABLE IF EXISTS loan;

CREATE TABLE Book (
    bookID INTEGER PRIMARY KEY,
    author VARCHAR (50),
    title VARCHAR (100) UNIQUE,
    price DOUBLE,
    inStore INTEGER
);

CREATE TABLE Member (
    member_id INTEGER PRIMARY KEY,
    firstName VARCHAR (25),
    lastName VARCHAR (25),
    registeredDate VARCHAR (25)
);

CREATE TABLE Loan (
    loanID INTEGER PRIMARY KEY,
    book INTEGER,
    member INTEGER,
    loanDate VARCHAR (25),
    returned BOOLEAN,
    FOREIGN KEY (book) REFERENCES Book(id),
    FOREIGN KEY (member) REFERENCES Member(member_id)
);