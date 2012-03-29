package org.siemac.metamac.gopestat.web.client.gin;

import org.siemac.metamac.gopestat.web.client.GopestatPlaceManager;
import org.siemac.metamac.gopestat.web.client.GopestatWebConstants;
import org.siemac.metamac.gopestat.web.client.GopestatWebMessages;
import org.siemac.metamac.gopestat.web.client.NameTokens;
import org.siemac.metamac.gopestat.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.gopestat.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.gopestat.web.client.family.view.FamilyListViewImpl;
import org.siemac.metamac.gopestat.web.client.family.view.FamilyViewImpl;
import org.siemac.metamac.gopestat.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.gopestat.web.client.instance.view.InstanceViewImpl;
import org.siemac.metamac.gopestat.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.gopestat.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.gopestat.web.client.operation.view.OperationListViewImpl;
import org.siemac.metamac.gopestat.web.client.operation.view.OperationViewImpl;
import org.siemac.metamac.gopestat.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.gopestat.web.client.view.MainPageViewImpl;
import org.siemac.metamac.gopestat.web.client.widgets.presenter.GopestatToolStripPresenterWidget;
import org.siemac.metamac.gopestat.web.client.widgets.view.GopestatToolStripViewImpl;

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
        install(new DefaultModule(GopestatPlaceManager.class));

        // Constants
        bindConstant().annotatedWith(DefaultPlace.class).to(NameTokens.familyListPage);

        // Presenters
        bindPresenter(MainPagePresenter.class, MainPagePresenter.MainPageView.class, MainPageViewImpl.class, MainPagePresenter.MainPageProxy.class);
        bindPresenter(FamilyListPresenter.class, FamilyListPresenter.FamilyListView.class, FamilyListViewImpl.class, FamilyListPresenter.FamiliesListProxy.class);
        bindPresenter(FamilyPresenter.class, FamilyPresenter.FamilyView.class, FamilyViewImpl.class, FamilyPresenter.FamilyProxy.class);
        bindPresenter(OperationListPresenter.class, OperationListPresenter.OperationListView.class, OperationListViewImpl.class, OperationListPresenter.OperationsListProxy.class);
        bindPresenter(OperationPresenter.class, OperationPresenter.OperationView.class, OperationViewImpl.class, OperationPresenter.OperationProxy.class);
        bindPresenter(InstancePresenter.class, InstancePresenter.InstanceView.class, InstanceViewImpl.class, InstancePresenter.InstanceProxy.class);

        // Presenter widgets
        bindSingletonPresenterWidget(GopestatToolStripPresenterWidget.class, GopestatToolStripPresenterWidget.GopestatToolStripView.class, GopestatToolStripViewImpl.class);

        // Interfaces
        bind(GopestatWebConstants.class).in(Singleton.class);
        bind(GopestatWebMessages.class).in(Singleton.class);
    }

}
