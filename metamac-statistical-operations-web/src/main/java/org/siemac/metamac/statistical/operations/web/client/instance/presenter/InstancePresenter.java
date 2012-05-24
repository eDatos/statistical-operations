package org.siemac.metamac.statistical.operations.web.client.instance.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCodeListsEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCodeListsEvent.UpdateCodeListsHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateFrequencyCodesEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateFrequencyCodesEvent.UpdateFrequencyCodesHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent.UpdateOperationsListsHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOrganisationSchemesEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOrganisationSchemesEvent.UpdateOrganisationSchemesHandler;
import org.siemac.metamac.statistical.operations.web.client.instance.view.handlers.InstanceUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.GetConceptsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetConceptsFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOrganisationsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOrganisationsFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceResult;
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.events.UpdateConceptSchemesEvent;
import org.siemac.metamac.web.common.client.events.UpdateConceptSchemesEvent.UpdateConceptSchemesHandler;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.dispatch.shared.DispatchAsync;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.ProxyEvent;
import com.gwtplatform.mvp.client.annotations.TitleFunction;
import com.gwtplatform.mvp.client.proxy.Place;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.gwtplatform.mvp.client.proxy.Proxy;
import com.gwtplatform.mvp.client.proxy.RevealContentEvent;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;

public class InstancePresenter extends Presenter<InstancePresenter.InstanceView, InstancePresenter.InstanceProxy>
        implements
            InstanceUiHandlers,
            UpdateCodeListsHandler,
            UpdateOrganisationSchemesHandler,
            UpdateOperationsListsHandler,
            UpdateConceptSchemesHandler,
            UpdateFrequencyCodesHandler {

    private final DispatchAsync                dispatcher;
    private final PlaceManager                 placeManager;

    private Long                               idOperation;
    private Long                               idInstance;
    private InstanceDto                        instanceDto;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object                 TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.instancePage)
    public interface InstanceProxy extends Proxy<InstancePresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbInstance();
    }

    public interface InstanceView extends View, HasUiHandlers<InstanceUiHandlers> {

        void setInstance(InstanceDto instanceDto);
        HasClickHandlers getSave();
        void onInstanceSaved(InstanceDto instanceDto);
        InstanceDto getInstance(InstanceDto instanceDto);

        void setOperationsLists(List<InstanceTypeDto> instanceTypeDtos, List<SurveySourceDto> surveySourceDtos, List<CollMethodDto> collMethodDtos, List<CostDto> costDtos);
        void setOrganisationScheme(List<ExternalItemBtDto> schemes);
        void setConceptScheme(List<ExternalItemBtDto> schemes);
        void setCodeLists(List<ExternalItemBtDto> codeLists);

        void setInfSuppliersOrg(List<ExternalItemBtDto> organisations);
        void setInfSuppliersConcept(List<ExternalItemBtDto> concepts);
        void setStatisticalUnitConcepts(List<ExternalItemBtDto> concepts);
        void setTemporalGranularityCodes(List<ExternalItemBtDto> codes);
        void setFreqCollCodes(List<ExternalItemBtDto> codes);

        boolean validate();
        HasClickHandlers getPublishInstanceInternally();
        HasClickHandlers getPublishInstanceExternally();
    }

    @Inject
    public InstancePresenter(EventBus eventBus, InstanceView instanceView, InstanceProxy instanceProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            OperationsToolStripPresenterWidget operationsToolStripPresenterWidget) {
        super(eventBus, instanceView, instanceProxy);
        this.dispatcher = dispatcher;
        this.placeManager = placeManager;
        this.operationsToolStripPresenterWidget = operationsToolStripPresenterWidget;
        getView().setUiHandlers(this);
    }

    @Override
    protected void revealInParent() {
        RevealContentEvent.fire(this, MainPagePresenter.TYPE_SetContextAreaContent, this);
    }

    @Override
    protected void onBind() {
        super.onBind();

        registerHandler(getView().getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveInstance(getView().getInstance(instanceDto));
                }
            }
        }));

        registerHandler(getView().getPublishInstanceInternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishInstanceInternally();
            }
        }));

        registerHandler(getView().getPublishInstanceExternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishInstanceExternally();
            }
        }));
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().instance());
        setInSlot(TYPE_SetContextAreaContentToolBar, operationsToolStripPresenterWidget);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
        super.prepareFromRequest(request);
        String id = request.getParameter(PlaceRequestParams.instanceParam, null);
        if (id != null) {
            idInstance = Long.valueOf(id);
            retrieveInstance(idInstance);
        }
        // Get operation id
        String operation = PlaceRequestUtils.getOperationParamFromUrl(placeManager);
        if (operation != null) {
            idOperation = Long.valueOf(operation);
        }
    }

    @Override
    public void saveInstance(InstanceDto instanceToSave) {
        dispatcher.execute(new SaveInstanceAction(idOperation, instanceToSave), new WaitingAsyncCallback<SaveInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(SaveInstanceResult result) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getMessageList(getMessages().instanceSaved()), MessageTypeEnum.SUCCESS);
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);
            }
        });
    }

    private void retrieveInstance(Long idInstance) {
        dispatcher.execute(new GetInstanceAction(idInstance), new WaitingAsyncCallback<GetInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetInstanceResult result) {
                instanceDto = result.getInstanceDto();
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleInstance(instanceDto.getCode()));
                getView().setInstance(instanceDto);
            }
        });
    }

    private void publishInstanceInternally() {
        dispatcher.execute(new PublishInternallyInstanceAction(instanceDto.getId()), new WaitingAsyncCallback<PublishInternallyInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorInternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(PublishInternallyInstanceResult result) {
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getMessageList(getMessages().instanceInternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void publishInstanceExternally() {
        dispatcher.execute(new PublishExternallyInstanceAction(instanceDto.getId()), new WaitingAsyncCallback<PublishExternallyInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorExternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(PublishExternallyInstanceResult result) {
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getMessageList(getMessages().instanceExternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOperationsLists(UpdateOperationsListsEvent event) {
        getView().setOperationsLists(event.getInstanceTypeDtos(), event.getSurveySourceDtos(), event.getCollMethodDtos(), event.getCostDtos());
    }

    @ProxyEvent
    @Override
    public void onUpdateOrganisationSchemes(UpdateOrganisationSchemesEvent event) {
        getView().setOrganisationScheme(event.getOrganisationSchemes());
    }

    @ProxyEvent
    @Override
    public void onUpdateConceptSchemes(UpdateConceptSchemesEvent event) {
        getView().setConceptScheme(event.getConceptSchemes());
    }

    @ProxyEvent
    @Override
    public void onUpdateCodeLists(UpdateCodeListsEvent event) {
        getView().setCodeLists(event.getCodeLists());
    }

    @Override
    public void populateInfSuppliersOrg(String schemeUri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(schemeUri), new WaitingAsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setInfSuppliersOrg(result.getOrganisations());
            }
        });
    }

    @Override
    public void populateInfSuppliersConcept(String schemeUri) {
        dispatcher.execute(new GetConceptsFromSchemeAction(schemeUri), new WaitingAsyncCallback<GetConceptsFromSchemeResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetConceptsFromSchemeResult result) {
                getView().setInfSuppliersConcept(result.getConcepts());
            }
        });
    }

    @Override
    public void populateStatisticalUnitConcepts(String schemeUri) {
        dispatcher.execute(new GetConceptsFromSchemeAction(schemeUri), new WaitingAsyncCallback<GetConceptsFromSchemeResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(InstancePresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetConceptsFromSchemeResult result) {
                getView().setStatisticalUnitConcepts(result.getConcepts());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateFrequencyCodes(UpdateFrequencyCodesEvent event) {
        getView().setTemporalGranularityCodes(event.getTemporalGranularityCodes());
        getView().setFreqCollCodes(event.getFreqCollCodes());
    }

}
