DROP TABLE IF EXISTS book;
DROP TABLE IF EXISTS member;
DROP TABLE IF EXISTS loan;

CREATE TABLE Book (
    id INTEGER PRIMARY KEY,
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
    book INTEGER,
    member INTEGER,
    loanDate VARCHAR (25),
    FOREIGN KEY (book) REFERENCES Book(id),
    FOREIGN KEY (member) REFERENCES Member(member_id)
);

-- INSERT INTO Book (author, title, price, inStore) VALUES ("sura", "tom and jerry", 2.99, TRUE);

-- INSERT INTO Book (author, price, inStore) VALUES ("abebe", "harry potter", 12.54, FALSE);

-- INSERT INTO Book (author, price, inStore) VALUES ("jemal", "mission impossible", 32.2, FALSE);