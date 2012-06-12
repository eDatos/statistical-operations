package org.siemac.metamac.statistical.operations.web.client.gin;

import org.siemac.metamac.statistical.operations.web.client.LoggedInGatekeeper;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.OperationsPlaceManager;
import org.siemac.metamac.statistical.operations.web.client.OperationsWebConstants;
import org.siemac.metamac.statistical.operations.web.client.OperationsWebMessages;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.view.FamilyListViewImpl;
import org.siemac.metamac.statistical.operations.web.client.family.view.FamilyViewImpl;
import org.siemac.metamac.statistical.operations.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.statistical.operations.web.client.instance.view.InstanceViewImpl;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.OperationListViewImpl;
import org.siemac.metamac.statistical.operations.web.client.operation.view.OperationViewImpl;
import org.siemac.metamac.statistical.operations.web.client.presenter.ErrorPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.presenter.UnauthorizedPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.view.ErrorPageViewImpl;
import org.siemac.metamac.statistical.operations.web.client.view.MainPageViewImpl;
import org.siemac.metamac.statistical.operations.web.client.view.UnauthorizedPageViewImpl;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.client.widgets.view.OperationsToolStripViewImpl;

import com.google.inject.Singleton;
import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;
import com.gwtplatform.mvp.client.gin.DefaultModule;

public class ClientModule extends AbstractPresenterModule {

    @Override
    protected void configure() {
        // Default implementation of standard resources
        // |_ bind(EventBus.class).to(SimpleEventBus.class).in(Singleton.class);
        // |_ bind(TokenFormatter.class).to(ParameterTokenFormatter.class).in(Singleton.class);
        // |_ bind(RootPresenter.class).asEagerSingleton();
        // |_ bind(PlaceManager.class).to(MyPlaceManager.class).in(Singleton.class);
        // |_ bind(GoogleAnalytics.class).to(GoogleAnalyticsImpl.class).in(Singleton.class);
        install(new DefaultModule(OperationsPlaceManager.class));

        // Constants
        bindConstant().annotatedWith(DefaultPlace.class).to(NameTokens.familyListPage);

        // Gate keeper
        bind(LoggedInGatekeeper.class).in(Singleton.class);

        // Presenters
        bindPresenter(MainPagePresenter.class, MainPagePresenter.MainPageView.class, MainPageViewImpl.class, MainPagePresenter.MainPageProxy.class);
        bindPresenter(FamilyListPresenter.class, FamilyListPresenter.FamilyListView.class, FamilyListViewImpl.class, FamilyListPresenter.FamiliesListProxy.class);
        bindPresenter(FamilyPresenter.class, FamilyPresenter.FamilyView.class, FamilyViewImpl.class, FamilyPresenter.FamilyProxy.class);
        bindPresenter(OperationListPresenter.class, OperationListPresenter.OperationListView.class, OperationListViewImpl.class, OperationListPresenter.OperationsListProxy.class);
        bindPresenter(OperationPresenter.class, OperationPresenter.OperationView.class, OperationViewImpl.class, OperationPresenter.OperationProxy.class);
        bindPresenter(InstancePresenter.class, InstancePresenter.InstanceView.class, InstanceViewImpl.class, InstancePresenter.InstanceProxy.class);

        // Error pages
        bindPresenter(ErrorPagePresenter.class, ErrorPagePresenter.ErrorPageView.class, ErrorPageViewImpl.class, ErrorPagePresenter.ErrorPageProxy.class);
        bindPresenter(UnauthorizedPagePresenter.class, UnauthorizedPagePresenter.UnauthorizedPageView.class, UnauthorizedPageViewImpl.class, UnauthorizedPagePresenter.UnauthorizedPageProxy.class);

        // Presenter widgets
        bindSingletonPresenterWidget(OperationsToolStripPresenterWidget.class, OperationsToolStripPresenterWidget.OperationsToolStripView.class, OperationsToolStripViewImpl.class);

        // Interfaces
        bind(OperationsWebConstants.class).in(Singleton.class);
        bind(OperationsWebMessages.class).in(Singleton.class);
    }

}
