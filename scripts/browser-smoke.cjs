const { chromium } = require('playwright');
const { expect } = require('playwright/test');
const http = require('http');
const fs = require('fs');
const path = require('path');
const assert = require('assert/strict');

(async () => {
  console.log('Browser smoke: starting local fixture server');
  const root = path.resolve(__dirname, '..');
  const server = http.createServer((request, response) => {
    const relative = request.url === '/' ? 'tests/browser/smoke.html' : request.url.slice(1).split('?')[0];
    const file = path.resolve(root, relative);
    if (!file.startsWith(root + path.sep)) { response.writeHead(403).end(); return; }
    try {
      const contentType = file.endsWith('.js') ? 'text/javascript' : file.endsWith('.css') ? 'text/css' : 'text/html';
      const content = fs.readFileSync(file);
      response.writeHead(200, { 'Content-Type': contentType }); response.end(content);
    } catch { response.writeHead(404).end(); }
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  let browserServer;
  try {
    const channel = process.env.BROWSER_CHANNEL || (process.platform === 'win32' ? 'msedge' : undefined);
    console.log('Browser smoke: launching '+(channel || 'chromium'));
    browserServer = await chromium.launchServer({ headless: true, timeout: 15000, ...(channel ? { channel } : {}) });
    browser = await chromium.connect(browserServer.wsEndpoint());
    const page = await browser.newPage({ viewport: { width: 1600, height: 1000 }, hasTouch: true });
    page.setDefaultTimeout(10000);
    await page.clock.install({ time: new Date('2026-10-28T09:00:00Z') });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(`http://127.0.0.1:${server.address().port}/`);
    console.log('Browser smoke: fixture loaded');
    await page.getByRole('tab', {name:'Alex',exact:true}).waitFor();
    await page.locator('.card-board-card-title').filter({hasText:'Urgent long'}).waitFor();
    await expect(page.locator('.card-board-card')).toHaveCount(5);
    await page.getByRole('tab',{name:'Alex',exact:true}).click();
    await expect(page.locator('.card-board-card')).toHaveCount(3);
    assert.equal(await page.locator('.card-board-card').filter({hasText:'Sam job'}).count(),0);
    const color = await page.locator('.card-board-card[data-assignee=Alex] svg polygon').first().evaluate(el=>getComputedStyle(el).fill);
    assert.equal(color,'rgb(195, 220, 250)');
    await page.evaluate(()=>{
      document.documentElement.style.setProperty('--text-normal','#e2e6ed');
      document.documentElement.style.setProperty('--background-primary','#202124');
      const changed=structuredClone(testSettings); changed.data.globalSettings.people[0].color='#112233';
      testApp.ports.interopToElm.send({tag:'settingsUpdated',data:changed});
    });
    await page.waitForFunction(()=>[...document.querySelectorAll('.card-board-card[data-assignee=Alex] .card-board-card-title')]
      .every(el=>getComputedStyle(el).color==='rgb(226, 230, 237)'));
    await page.evaluate(()=>{
      document.documentElement.style.removeProperty('--text-normal');
      document.documentElement.style.removeProperty('--background-primary');
      testApp.ports.interopToElm.send({tag:'settingsUpdated',data:testSettings});
    });
    const border = await page.locator('.card-board-card').filter({hasText:'Urgent long'}).locator('.card-board-card-highlight-area').getAttribute('class');
    assert.match(border,/important/);
    assert.equal(await page.locator('.card-board-card').filter({hasText:'Urgent long'}).locator('.card-board-card-highlight-area').evaluate(el=>getComputedStyle(el).backgroundColor),'rgb(219, 150, 0)');
    await page.getByRole('tab',{name:'All people',exact:true}).click();
    await page.getByRole('button',{name:'Add card to Todo',exact:true}).click();
    await page.waitForFunction(()=>outbound.some(message=>message.tag==='createCard'));
    const urgentCard=page.locator('.card-board-card').filter({hasText:'Urgent long'});
    await urgentCard.getByRole('button',{name:'Assign person and colour'}).click();
    await page.waitForFunction(()=>outbound.some(message=>message.tag==='assignCard'));
    await page.getByRole('heading',{name:'Assign people and colours',exact:true}).waitFor();
    const assigneeSelect=page.getByLabel('Choose an assigned person').first();
    assert.deepEqual(await assigneeSelect.locator('option').evaluateAll(options=>options.map(option=>option.value)),['','Alex','Sam']);
    await assigneeSelect.selectOption('Sam');
    await expect(page.locator('.modal input:not([type=color])').first()).toHaveValue('Sam');
    await page.getByRole('button',{name:'Cancel',exact:true}).click();
    await urgentCard.getByRole('button',{name:'Edit estimate and urgency'}).click();
    await page.waitForFunction(()=>outbound.some(message=>message.tag==='editPlanning'));
    const drag = async (card, destination, touch = false) => {
      const from = await card.boundingBox();
      const to = await destination.boundingBox();
      const start = { x: from.x + (touch ? from.width/2 : 2), y: from.y + from.height/2 };
      const end = { x: to.x + to.width/2, y: to.y + 8 };
      if (touch) {
        const cdp = await page.context().newCDPSession(page);
        await cdp.send('Input.dispatchTouchEvent',{type:'touchStart',touchPoints:[start]});
        await page.clock.runFor(500);
        await expect(card).toHaveClass(/card-board-pointer-dragging/);
        for(let step=1;step<=12;step++) await cdp.send('Input.dispatchTouchEvent',{type:'touchMove',touchPoints:[{x:start.x+(end.x-start.x)*step/12,y:start.y+(end.y-start.y)*step/12}]});
        await cdp.send('Input.dispatchTouchEvent',{type:'touchEnd',touchPoints:[]});
        await cdp.detach();
      } else {
        await page.mouse.move(start.x,start.y); await page.mouse.down();
        await page.mouse.move(end.x,end.y,{steps:12}); await page.mouse.up();
      }
    };
    const todo = page.locator('[data-card-column-index="0"]');
    await drag(page.locator('.card-board-card').filter({hasText:'Quick report'}), todo.locator('.card-board-column-list'), true);
    await page.waitForFunction(()=>testSettings.data.globalSettings.cardOrder?.length===1);
    await expect(todo.locator('.card-board-card-title').first()).toHaveText('Quick report');
    await drag(urgentCard,page.locator('[data-card-column-index="1"] .card-board-column-list'));
    await page.waitForFunction(()=>outbound.some(message=>message.tag==='reorderCard' && message.data.columnIndex===1));
    await page.waitForFunction(()=>testMarkdown.split('\n').find(line=>line.includes('Urgent long')).includes('#status/doing'));
    await page.getByRole('button',{name:'Smart schedule view',exact:true}).click();
    await expect(page.locator('.card-board-schedule-day')).toHaveCount(5);
    await expect(page.locator('.card-board-schedule-slot.is-late')).toHaveCount(2);
    assert.equal(await page.locator('.card-board-schedule-day[aria-label="2026-10-28"]').locator('.card-board-schedule-slot').count(),2);
    await page.getByRole('heading',{name:'Needs estimate (1)',exact:true}).waitFor();
    await page.getByRole('heading',{name:'Needs assignee (1)',exact:true}).waitFor();
    await page.getByRole('tab',{name:'Sam',exact:true}).click();
    await expect(page.locator('.card-board-schedule-slot')).toHaveCount(1);
    await page.getByRole('tab',{name:'All people',exact:true}).click();
    const screenshot = path.join(root,'elm-stuff','schedule-smoke.png');
    await page.screenshot({path:screenshot,fullPage:true});
    await page.getByRole('button',{name:'Next week',exact:true}).click();
    await expect(page.locator('.card-board-schedule-day').first()).toHaveAttribute('aria-label','2026-11-02');
    await page.getByRole('button',{name:'Previous week',exact:true}).click();
    await page.setViewportSize({width:480,height:850});
    assert.equal(await page.locator('.card-board-schedule-day').count(),5);
    assert.equal(await page.locator('body').evaluate(el=>el.scrollWidth<=window.innerWidth),true);
    await page.setViewportSize({width:1600,height:1000});
    await page.getByRole('button',{name:'Kanban view',exact:true}).click();
    const complete = async (name, dismissal, hours) => {
      const card = page.locator('.card-board-card').filter({hasText:name});
      await card.locator('input[type=checkbox]').first().click();
      await page.getByRole('heading',{name:'How long did this task take?',exact:true}).waitFor();
      if (hours) await page.locator('.modal input').fill(String(hours));
      if(dismissal==='outside') await page.mouse.click(5,5);
      else if(dismissal==='escape') await page.keyboard.press('Escape');
      else await page.getByRole('button',{name:dismissal,exact:true}).click();
      await expect(page.locator('.modal')).toHaveCount(0);
      await page.waitForFunction(({name,hours})=>testMarkdown.split('\n').some(line=>line.includes(name)&&line.includes('@takes('+hours+'h)')&&line.includes('[x]')),{name,hours:hours||1});
    };
    await complete('Estimate me','Cancel');
    for(const [name,method,hours] of [['Outside duration','outside',null],['Escape duration','escape',null],['Entered duration','Complete task',2.5]]) {
      await page.evaluate(name=>{testMarkdown+='- [ ] '+name+' #status/todo\n';fixturePublish();},name);
      await page.locator('.card-board-card-title').filter({hasText:name}).waitFor();
      await complete(name,method,hours);
    }
    await page.evaluate(()=>{testMarkdown+='- [x] Shared complete @takes(4h) @assigned(Alex) @assigned(Sam) @completed(2026-10-28T12:00:00)\n- [ ] Pair colours @assigned(Alex) @assigned(Sam) #status/todo\n- [ ] Triple colours @assigned(Alex) @assigned(Sam) @assigned(Jo) #status/todo\n- [ ] Four people @assigned(Alex) @assigned(Sam) @assigned(Jo) @assigned(Riley) @takes(8h) #status/todo\n';fixturePublish();});
    const four = page.locator('.card-board-card').filter({hasText:'Four people'});
    await expect(four.locator('.card-board-assignee-background polygon')).toHaveCount(4);
    await expect(page.locator('.card-board-card').filter({hasText:'Pair colours'}).locator('.card-board-assignee-background polygon')).toHaveCount(2);
    await expect(page.locator('.card-board-card').filter({hasText:'Triple colours'}).locator('.card-board-assignee-background polygon')).toHaveCount(3);
    await four.screenshot({path:path.join(root,'elm-stuff','four-person-card.png')});
    await page.getByRole('tab',{name:'Riley',exact:true}).click();
    await expect(page.locator('.card-board-card')).toHaveCount(1);
    await page.getByRole('tab',{name:'All people',exact:true}).click();
    await page.getByRole('button',{name:'Activity view',exact:true}).click();
    await page.getByRole('heading',{name:'Completed work across the vault'}).waitFor();
    await expect(page.locator('.card-board-activity-grid button')).toHaveCount(371);
    await page.getByRole('tab',{name:'Sam',exact:true}).click();
    await expect(page.locator('.card-board-activity-bar-row')).toHaveCount(1);
    await expect(page.locator('.card-board-activity-bar-row')).toContainText('Sam');
    await page.getByRole('button',{name:'Show task hours',exact:true}).click();
    await expect(page.locator('.card-board-activity-bar-row')).toHaveText('Sam2');
    await page.getByRole('button',{name:'2026-10-28: 2 hours',exact:true}).click();
    await expect(page.locator('[aria-live="polite"]')).toHaveText('2026-10-28: 2 hours');
    await page.screenshot({path:path.join(root,'elm-stuff','activity-smoke.png'),fullPage:true});
    await page.getByRole('button',{name:'Back to board',exact:true}).click();
    await page.getByRole('tab',{name:'All people',exact:true}).click();
    const saved = await page.evaluate(()=>({settings:testSettings, markdown:testMarkdown}));
    await page.reload();
    await page.waitForFunction(()=>window.fixtureReady);
    await page.evaluate(saved=>{window.testSettings=saved.settings;window.testMarkdown=saved.markdown;testApp.ports.interopToElm.send({tag:'settingsUpdated',data:saved.settings});fixturePublish();},saved);
    await expect(page.locator('[data-card-column-index="0"] .card-board-card-title').first()).toHaveText('Quick report');
    await page.evaluate(()=>{testMarkdown+='- [ ] Killian estimated @assigned(Killian) @takes(4h) #status/todo\n';fixturePublish();});
    await page.locator('.card-board-card').filter({hasText:'Killian estimated'}).locator('input[type=checkbox]').first().click();
    await page.getByRole('heading',{name:'How long did this task actually take?',exact:true}).waitFor();
    await page.locator('.modal input').fill('3');
    await page.getByRole('button',{name:'Complete task',exact:true}).click();
    await page.waitForFunction(()=>testMarkdown.includes('@takes(4h)') && testMarkdown.includes('@spent(3h)'));
    for(const [name,estimate,method,expected] of [['Killian canceled','@takes(2h)','cancel',2],['Killian outside','@takes(0.5d)','outside',4],['Killian no estimate','','escape',1]]) {
      await page.evaluate(({name,estimate})=>{testMarkdown+='- [ ] '+name+' @assigned(killian) '+estimate+' #status/todo\n';fixturePublish();},{name,estimate});
      await page.locator('.card-board-card').filter({hasText:name}).locator('input[type=checkbox]').first().click();
      await page.getByRole('heading',{name:'How long did this task actually take?',exact:true}).waitFor();
      await page.locator('.modal input').fill('99');
      if(method==='outside')await page.mouse.click(5,5); else if(method==='escape')await page.keyboard.press('Escape'); else await page.getByRole('button',{name:'Cancel',exact:true}).click();
      await page.waitForFunction(({name,expected})=>testMarkdown.split('\n').some(line=>line.includes(name)&&line.includes('@spent('+expected+'h)')),{name,expected});
    }
    await page.getByRole('button',{name:'Activity view',exact:true}).click();
    await page.getByRole('tab',{name:'Killian',exact:true}).click();
    await page.getByRole('button',{name:'Show task hours',exact:true}).click();
    await expect(page.locator('.card-board-feedback-chart polyline')).toHaveCount(2);
    await expect(page.locator('.card-board-estimate-legend')).toHaveText('Estimated: 11h');
    await expect(page.locator('.card-board-actual-legend')).toHaveText('Actual: 10h');
    await page.screenshot({path:path.join(root,'elm-stuff','killian-feedback.png'),fullPage:true});
    await page.getByRole('button',{name:'Back to board',exact:true}).click();
    await page.getByRole('tab',{name:'All people',exact:true}).click();
    await page.locator('.card-board-card').filter({hasText:'Four people'}).getByRole('button',{name:'Assign person and colour',exact:true}).click();
    await page.getByRole('heading',{name:'Assign people and colours',exact:true}).waitFor();
    await expect(page.getByLabel('Choose an assigned person').first()).toBeVisible();
    const peopleOptions=await page.getByLabel('Choose an assigned person').first().locator('option').evaluateAll(options=>options.map(option=>option.value));
    const normalizedPeople=peopleOptions.map(name=>name.toLowerCase());
    assert.ok(normalizedPeople.includes('jo')&&normalizedPeople.includes('riley')&&normalizedPeople.includes('killian'),JSON.stringify(peopleOptions));
    await page.getByRole('button',{name:'Cancel',exact:true}).click();
    const background = await page.evaluate(async()=>{
      const source=await(await fetch('/elm-stuff/smoke-worker.js')).text();
      const parser=createBackgroundParser(source,{dataviewTaskCompletion:{taskCompletionTracking:false,taskCompletionUseEmojiShorthand:false,taskCompletionText:'completion'}});
      let beats=0;let added=0;let count=0;let snapshotReady;const snapshot=new Promise(resolve=>snapshotReady=resolve);
      const timer=setInterval(()=>beats++,5);
      parser.ports.interopFromElm.subscribe(message=>{if(message.tag==='tasksAdded')added++;if(message.tag==='allTaskItems'){count=message.data.length;snapshotReady();}});
      const content=Array.from({length:10},(_,i)=>'- [ ] Work '+i+' @takes(2h) @assigned(Killian)').join('\n');
      for(let i=0;i<200;i++){
        parser.ports.interopToElm.send({tag:'fileAdded',data:{filePath:'Performance'+i+'.md',fileDate:null,currentYear:2026,fileContents:content}});
        if(i%32===31)await parser.flush();
      }
      parser.ports.interopToElm.send({tag:'allMarkdownLoaded',data:{}});
      parser.ports.interopToElm.send({tag:'viewInitialized'});await parser.flush();await snapshot;
      clearInterval(timer);parser.terminate();return {beats,added,count};
    });
    assert.equal(background.count,2000);assert.equal(background.added,0);assert.ok(background.beats>0,'UI timers must run while background indexing is active');
    console.log('Background parsing: '+JSON.stringify(background));
    if(process.env.PRODUCTION_SMOKE==='1') {
      const production = await page.evaluate(async()=>runProductionSmoke(await(await fetch('/main.js')).text()));
      assert.equal(production.startupReads,0);assert.equal(production.reads,32);assert.equal(production.count,32);
      console.log('Production bundle startup: '+JSON.stringify(production));
    }
    assert.deepEqual(await page.evaluate(()=>fixtureNotices),[]);
    assert.deepEqual(errors,[]);
    console.log('Browser smoke passed: touch reorder, persisted anchors, multi-person regions, completion entry/cancel/backdrop/Escape, activity totals and heatmap;  tabs, colours, deadline border, creation/assignment/planning ports, drag/drop, five-day schedule, late highlights, missing metadata, week navigation and narrow layout.');
    console.log('Screenshot: '+screenshot);
  } catch (error) {
    console.error(error);
    throw error;
  } finally {
    if(browser)await browser.close();
    if(browserServer)await browserServer.kill();
    server.closeAllConnections();
    await new Promise(resolve=>server.close(resolve));
  }
})().catch(error=>{console.error(error);process.exitCode=1;});
