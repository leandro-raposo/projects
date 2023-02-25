const puppeteer = require('puppeteer');
const createCsvWriter = require('csv-writer').createObjectCsvWriter;

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto('https://www.instagram.com/p/xxxxxx/'); // substitua "xxxxxx" pelo código da publicação desejada

  // extraia os comentários usando a função querySelectorAll
  const comments = await page.$$eval('.Mr508', elements => elements.map(el => el.textContent));

  // estruture os dados em um formato JSON ou array
  const commentData = comments.map(comment => ({
    text: comment,
  }));

  // armazene os dados em um arquivo CSV usando csv-writer
  const csvWriter = createCsvWriter({
    path: 'comments.csv',
    header: [
      {id: 'text', title: 'Comment Text'},
    ],
  });
  await csvWriter.writeRecords(commentData);

  await browser.close();
})();
