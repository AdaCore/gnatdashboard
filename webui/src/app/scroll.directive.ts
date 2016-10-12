import {
    Directive, ElementRef, HostListener, Input, Output, Renderer
} from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs/Subscription';

@Directive({ selector: '[auto-scroll]' })
export class AutoScroll {
    @Input() routerLink: any = null;
    @Input() href: string = null;
    @Input() smooth: boolean = false;

    private document: Document = null;
    private body: HTMLBodyElement = null;

    constructor(
        el: ElementRef,
        private router: Router,
        private renderer: Renderer)
    {
        this.document = el.nativeElement.ownerDocument;
        this.body = el.nativeElement.ownerDocument.body;
    }

    @HostListener('click', ['$event'])
    private handleClick(event: Event): boolean {
        if (this.routerLink) {
            // We need to navigate there first.
            // Navigation is handled by the routerLink directive,
            // so we only need to listen for route change.
            // Note: the change event is also emitted when navigating to the
            // current route again.
            let subscription: Subscription =
                <Subscription>this.router.events.subscribe(() => {
                    subscription.unsubscribe();
                    this.scrollView(this.href);
                });
        } else {
            this.scrollView(this.href);
        }
        return false; // calls preventDefault()
    }

    private scrollView(anchor: string): void {
        let anchorTarget: HTMLElement =
            this.document.getElementById(anchor.substr(1));
        if (anchorTarget !== null) {
            (this.smooth ? this.doSmoothScroll : this.doScroll)(anchorTarget);
            this.flashAnchorTarget(anchorTarget);
        }
    }

    /**
     * TODO(delay): implement a more generic approach to flashing elements of
     * the DOM.
     */
    private flashAnchorTarget(target: HTMLElement): void {
        this.renderer.setElementStyle(target, 'backgroundColor', '#f8eec7');

        setTimeout(() => {
            this.renderer.setElementStyle(
                target, 'transition', 'background-color 650ms linear');
            this.renderer.setElementStyle(target, 'backgroundColor', null);
            setTimeout(() => {
                this.renderer.setElementStyle(target, 'transition', null);
            }, 1000 /* remove the transition effect AFTER it has completed */);
        }, 3000 /* flash for 3s */);
    }

    /**
     * Scroll |container| so that |target| is visible.
     *
     * @param container The DOM element to scroll.
     * @param target A child element of container to scroll to.
     */
    private static setScrollTop(container, target: HTMLElement): void {
        if (container && 'scrollTop' in container) {
            container.scrollTop =
                target.offsetTop -
                target.scrollTop +
                target.clientTop;
        }
    }

    /**
     * Scroll |target| into view by jumping to it (instant scroll).
     *
     * @param target The element to reveal by scrolling.
     * @see |doSmoothScroll| for an animated scroll.
     */
    protected doScroll(target: HTMLElement): void {
        AutoScroll.setScrollTop(this.body, target);
        AutoScroll.setScrollTop(this.document.documentElement, target);
        AutoScroll.setScrollTop(this.document.body.parentNode, target);
    }

    /**
     * Compute the current scroll amount.
     *
     * @return The number of pixel the page is scrolled down.
     */
    private static currentYPosition(): number {
        if ('pageYOffset' in self) {
            // Firefox, Chrome, Opera, Safari
            return self.pageYOffset;
        }
        if ('documentElement' in document &&
            'scrollTop' in document.documentElement) {
            // Internet Explorer 6 - standards mode
            return document.documentElement.scrollTop;
        }
        if ('scrollTop' in document.body) {
            // Internet Explorer 6, 7 and 8
            return document.body.scrollTop;
        }
        return 0;
    }

    /**
     * Compute the height in pixel an element is at in an HTML page.
     *
     * @param elm The element to find the position for.
     * @return The number of pixels down the element is at.
     */
    private static elmYPosition(elm: HTMLElement): number {
        let y = elm.offsetTop;
        let node = elm;
        while (node.offsetParent && node.offsetParent !== document.body) {
            node = <HTMLElement>node.offsetParent;
            y += node.offsetTop;
        }
        return y;
    }

    /**
     * Same a |doScroll|, but with an animation.
     *
     * @param target The element to reveal by scrolling.
     */
    private doSmoothScroll(target: HTMLElement): void {
        const startY = AutoScroll.currentYPosition();
        const stopY = AutoScroll.elmYPosition(target);
        let distance = stopY > startY ? stopY - startY : startY - stopY;
        if (distance < 100) {
            scrollTo(0, stopY);
            return;
        }

        const speed = distance < 2000 ? Math.round(distance / 100) : 20;
        const step = Math.round(distance / 25);
        let leapY = stopY > startY ? startY + step : startY - step;
        let timer = 0;

        if (stopY > startY) {
            for (let i = startY; i < stopY; i += step, ++timer) {
                setTimeout(`window.scrollTo(0, ${leapY})`, timer * speed);
                leapY += step;
                if (leapY > stopY) {
                    leapY = stopY;
                }
            }
        } else {
            for (let i = startY; i > stopY; i -= step, ++timer) {
                setTimeout(`window.scrollTo(0, ${leapY})`, timer * speed);
                leapY -= step;
                if (leapY < stopY) {
                    leapY = stopY;
                }
            }
        }

    }
}
