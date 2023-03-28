-- Book data
INSERT INTO Book (author, title, price, inStore)
VALUES ("JK rowlings", "Harry potter", 15, 2);

INSERT INTO Book (author, title, price, inStore)
VALUES ("George RR Martins", "Game of thrones", 22, 10);

INSERT INTO Book (author, title, price, inStore)
VALUES ("J.K. Rowling", "The Casual Vacancy", 18, 5);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Stephenie Meyer", "Twilight", 12, 8);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Dan Brown", "The Da Vinci Code", 20, 3);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Agatha Christie", "Murder on the Orient Express", 15, 6);

INSERT INTO Book (author, title, price, inStore)
VALUES ("J.R.R. Tolkien", "The Lord of the Rings", 25, 2);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Harper Lee", "To Kill a Mockingbird", 14, 4);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Ernest Hemingway", "The Old Man and the Sea", 16, 7);

INSERT INTO Book (author, title, price, inStore)
VALUES ("Gabriel García Márquez", "One Hundred Years of Solitude", 19, 9);


-- Member data
INSERT INTO Member (member_id, firstName, lastName, registeredDate)
VALUES (1234567890, "John", "Doe", '2023-03-17 07:26:55.9548435 UTC');

INSERT INTO Member (member_id, firstName, lastName, registeredDate)
VALUES (9876543210, "Jane", "Smith", '2023-03-18 12:30:45.3645821 UTC');

INSERT INTO Member (member_id, firstName, lastName, registeredDate)
VALUES (4567890123, "David", "Lee", '2023-03-19 18:20:15.9873210 UTC');

INSERT INTO Member (member_id, firstName, lastName, registeredDate)
VALUES (7890123456, "Amy", "Chen", '2023-03-20 09:45:30.6541234 UTC');

INSERT INTO Member (member_id, firstName, lastName, registeredDate)
VALUES (2345678901, "Robert", "Johnson", '2023-03-21 15:10:20.1234567 UTC');

-- Loan data

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (1, 1234567890, '2023-04-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (1, 2345678901, '2023-04-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (2, 1234567890, '2023-04-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (2, 7890123456, '2023-08-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (2, 4567890123, '2023-02-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (3, 1234567890, '2023-04-11 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (3, 9876543210, '2023-04-09 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (4, 1234567890, '2023-03-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (4, 2345678901, '2023-01-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (4, 7890123456, '2023-09-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (4, 4567890123, '2023-09-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (4, 9876543210, '2022-05-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (5, 7890123456, '2022-04-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (5, 1234567890, '2023-05-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (6, 2345678901, '2023-05-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (6, 7890123456, '2023-04-11 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (6, 9876543210, '2023-04-11 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (7, 4567890123, '2023-04-01 10:31:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (7, 9876543210, '2023-04-01 11:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (8, 1234567890, '2023-04-10 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (8, 7890123456, '2023-10-01 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (8, 4567890123, '2023-04-04 10:30:00.0000000 UTC', false);

INSERT INTO Loan (book, member, loanDate, returned)
VALUES (9, 2345678901, '2023-04-01 10:30:00.0000000 UTC', false);
